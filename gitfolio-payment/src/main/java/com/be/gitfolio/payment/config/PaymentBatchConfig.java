package com.be.gitfolio.payment.config;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.payment.domain.Payment;
import com.be.gitfolio.payment.domain.PaymentStatusHistory;
import com.be.gitfolio.payment.infrastructure.PaymentStatusHistoryRepository;
import com.be.gitfolio.payment.service.PaymentJobCompletionListener;
import com.be.gitfolio.payment.service.port.KakaoClient;
import com.be.gitfolio.payment.type.PaymentStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.ItemWriter;
import org.springframework.batch.item.database.BeanPropertyItemSqlParameterSourceProvider;
import org.springframework.batch.item.database.JdbcBatchItemWriter;
import org.springframework.batch.item.database.JdbcPagingItemReader;
import org.springframework.batch.item.database.support.SqlPagingQueryProviderFactoryBean;
import org.springframework.batch.item.support.ClassifierCompositeItemWriter;
import org.springframework.classify.Classifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.retry.backoff.FixedBackOffPolicy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.transaction.PlatformTransactionManager;

import javax.sql.DataSource;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.Map;

import static com.be.gitfolio.common.event.KafkaEvent.*;

@Configuration
@EnableBatchProcessing
@RequiredArgsConstructor
public class PaymentBatchConfig {

    private final JobRepository jobRepository;
    private final KafkaTemplate<String, ExpirationEvent> kafkaTemplate;
    private final KakaoClient kakaoClient;
    private final KakaoPayProperties payProperties;
    private final PaymentStatusHistoryRepository paymentStatusHistoryRepository;
    private final DataSource dataSource; // JDBC 사용을 위해 DataSource 추가

    @Bean
    public Job paymentExpirationJob(Step paymentExpirationStep, PaymentJobCompletionListener jobCompletionListener) {
        return new JobBuilder("paymentExpirationJob", jobRepository)
                .start(paymentExpirationStep)
                .listener(jobCompletionListener)
                .build();
    }

    @Bean
    public Step paymentExpirationStep(PlatformTransactionManager transactionManager) {
        return new StepBuilder("paymentExpirationStep", jobRepository)
                .<Payment, Payment>chunk(500, transactionManager)
                .reader(expiredPaymentItemReader())
                .processor(expiredPaymentItemProcessor())
                .writer(expiredPaymentItemWriter())
                .taskExecutor(taskExecutor())   // 병렬처리를 위해 TaskExecutor 사용
                .faultTolerant()
                .retryLimit(10)
                .retry(SQLException.class)
                .retry(IOException.class)
                .backOffPolicy(new FixedBackOffPolicy() {{
                    setBackOffPeriod(2000); // 재시도 간격 2초
                }})
                .build();
    }

    @Bean
    public TaskExecutor taskExecutor() {
        ThreadPoolTaskExecutor taskExecutor = new ThreadPoolTaskExecutor();
        taskExecutor.setCorePoolSize(2);    // 최소 스레드 수
        taskExecutor.setMaxPoolSize(4);    // 최대 스레드 수
        taskExecutor.setQueueCapacity(100); // 대기열 용량
        taskExecutor.setThreadNamePrefix("Batch-Thread-");
        taskExecutor.initialize();
        return taskExecutor;
    }

    // JDBC 기반 ZeroOffset 방식 ItemReader 설정
    @Bean
    public JdbcPagingItemReader<Payment> expiredPaymentItemReader() {
        JdbcPagingItemReader<Payment> reader = new JdbcPagingItemReader<>();
        reader.setDataSource(dataSource);
        reader.setFetchSize(500);
        reader.setRowMapper(new PaymentRowMapper()); // 커스텀 RowMapper 설정

        // 초기 값으로 마지막 조회된 ID를 0으로 설정
        Map<String, Object> parameterValues = new HashMap<>();
        parameterValues.put("lastFetchedId", 0L);
        parameterValues.put("endDate", LocalDate.now());
        parameterValues.put("canceled", PaymentStatus.CANCELED.toString());
        parameterValues.put("approved", PaymentStatus.APPROVED.toString());
        reader.setParameterValues(parameterValues);

        try {
            SqlPagingQueryProviderFactoryBean queryProvider = new SqlPagingQueryProviderFactoryBean();
            queryProvider.setDataSource(dataSource);
            queryProvider.setSelectClause("SELECT *");
            queryProvider.setFromClause("FROM payment");
            queryProvider.setWhereClause(
                    "WHERE payment_id > :lastFetchedId " +
                            "AND end_date < :endDate " +
                            "AND (status = :canceled OR status = :approved)"
            );
            queryProvider.setSortKey("payment_id"); // 다음 페이지를 위한 기준 컬럼
            reader.setQueryProvider(queryProvider.getObject());
        } catch (Exception e) {
            throw new RuntimeException("Failed to create query provider for ZeroOffsetItemReader", e);
        }

        return reader;
    }

    // 커스텀 RowMapper 구현 (Builder 패턴 사용)
    public static class PaymentRowMapper implements RowMapper<Payment> {
        @Override
        public Payment mapRow(ResultSet rs, int rowNum) throws SQLException {
            return Payment.builder()
                    .id(rs.getLong("payment_id"))
                    .startDate(rs.getDate("start_date").toLocalDate())
                    .endDate(rs.getDate("end_date").toLocalDate())
                    .status(PaymentStatus.valueOf(rs.getString("status")))
                    .memberId(rs.getLong("member_id"))
                    .transactionId(rs.getString("transaction_id"))
                    .sid(rs.getString("sid"))
                    .paidPlan(PaidPlan.valueOf(rs.getString("paid_plan")))
                    .paymentType(rs.getString("payment_type"))
                    .cardApprovedId(rs.getString("card_approved_id"))
                    .cardType(rs.getString("card_type"))
                    .cardCorp(rs.getString("card_corp"))
                    .amount(rs.getBigDecimal("amount"))
                    .build();
        }
    }

    @Bean
    public ItemProcessor<Payment, Payment> expiredPaymentItemProcessor() {
        return payment -> {
            if (payment.getStatus() == PaymentStatus.APPROVED) {
                // Approved면 정기결제 요청
                 boolean success = processRecurringPayment(payment);
                 if (success) {
                     payment.extendEndDate(); // 성공한 경우만 endDate 연장
                 }
                payment.extendEndDate();
            } else if (payment.getStatus() == PaymentStatus.CANCELED) {
                // CANCELED 상태면 ExpirationEvent 이벤트 발행
                ExpirationEvent event = new ExpirationEvent(payment.getMemberId());
                kafkaTemplate.send("expiration-topic", payment.getMemberId().toString(), event);
            }
            return payment;
        };
    }

    // JDBC 기반 Writer 설정: 결제 정보 업데이트
    @Bean
    public JdbcBatchItemWriter<Payment> updatePaymentItemWriter() {
        JdbcBatchItemWriter<Payment> writer = new JdbcBatchItemWriter<>();
        writer.setDataSource(dataSource);
        writer.setSql("UPDATE payment SET end_date = :endDate WHERE payment_id = :id");
        writer.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
        writer.setAssertUpdates(false); // 업데이트된 행이 없더라도 예외를 발생시키지 않음
        return writer;
    }

    // JDBC 기반 Writer 설정: 취소된 결제 삭제
    @Bean
    public JdbcBatchItemWriter<Payment> deleteCanceledPaymentsWriter() {
        JdbcBatchItemWriter<Payment> writer = new JdbcBatchItemWriter<>();
        writer.setDataSource(dataSource);
        writer.setSql("DELETE FROM payment WHERE payment_id = :id");
        writer.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
        writer.setAssertUpdates(false); // 업데이트된 행이 없더라도 예외를 발생시키지 않음
        return writer;
    }

    @Bean
    public ClassifierCompositeItemWriter<Payment> expiredPaymentItemWriter() {
        ClassifierCompositeItemWriter<Payment> classifierCompositeItemWriter = new ClassifierCompositeItemWriter<>();

        classifierCompositeItemWriter.setClassifier(new Classifier<Payment, ItemWriter<? super Payment>>() {
            @Override
            public ItemWriter<? super Payment> classify(Payment payment) {
                if (payment.getStatus() == PaymentStatus.APPROVED) {
                    return updatePaymentItemWriter(); // APPROVED 상태 처리 Writer
                } else if (payment.getStatus() == PaymentStatus.CANCELED) {
                    return deleteCanceledPaymentsWriter(); // CANCELED 상태 처리 Writer
                }
                throw new IllegalStateException("Unexpected Payment Status: " + payment.getStatus());
            }
        });

        return classifierCompositeItemWriter;
    }

    /**
     * 카카오페이에 정기결제 요청
     */
    private boolean processRecurringPayment(Payment payment) {
        try {
            Map<String, Object> parameters = Map.of(
                    "cid", "TCSUBSCRIP",
                    "sid", payment.getSid(),
                    "partner_order_id", "order_" + payment.getId(),
                    "partner_user_id", String.valueOf(payment.getMemberId()),
                    "item_name", payment.getPaidPlan().getPlanName(),
                    "quantity", 1,
                    "total_amount", payment.getPaidPlan().getCost(),
                    "vat_amount", 0,
                    "tax_free_amount", 0
            );

            kakaoClient.sendKakaoPaymentRequest(
                    "https://open-api.kakaopay.com/online/v1/payment/subscription", // URL
                    parameters, // 요청 파라미터
                    Void.class, // 응답 타입 (Bodyless)
                    payProperties.getSecretKey()
            );

            return true; // 결제 성공
        } catch (Exception e) {
            // 정기결제 실패 시 PaymentStatusHistory에 기록
            paymentStatusHistoryRepository.save(
                    PaymentStatusHistory.fail(payment.getId(), e.getMessage())
            );
            return false; // 결제 실패
        }
    }
}