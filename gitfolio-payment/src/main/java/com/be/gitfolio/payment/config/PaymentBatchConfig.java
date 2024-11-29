package com.be.gitfolio.payment.config;

import com.be.gitfolio.payment.domain.Payment;
import com.be.gitfolio.payment.domain.PaymentStatusHistory;
import com.be.gitfolio.payment.repository.PaymentRepository;
import com.be.gitfolio.payment.repository.PaymentStatusHistoryRepository;
import com.be.gitfolio.payment.service.PaymentJobCompletionListener;
import com.be.gitfolio.payment.type.PaymentStatus;
import jakarta.persistence.EntityManagerFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.ItemWriter;
import org.springframework.batch.item.database.JpaCursorItemReader;
import org.springframework.batch.item.database.JpaItemWriter;
import org.springframework.batch.item.support.CompositeItemWriter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.web.reactive.function.client.WebClient;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Map;

import static com.be.gitfolio.common.event.KafkaEvent.*;

@Configuration
@EnableBatchProcessing
@RequiredArgsConstructor
public class PaymentBatchConfig {

    private final JobRepository jobRepository;
    private final EntityManagerFactory entityManagerFactory;
    private final KafkaTemplate<String, ExpirationEvent> kafkaTemplate;
    private final WebClient kakaoWebClient;
    private final KakaoPayProperties payProperties;
    private final PaymentStatusHistoryRepository paymentStatusHistoryRepository;
    private final PaymentRepository paymentRepository;

    private HttpHeaders getHeaders() {
        HttpHeaders headers = new HttpHeaders();
        String auth = "SECRET_KEY " + payProperties.getSecretKey();
        headers.set("Authorization", auth);
        headers.set("Content-Type", "application/json");
        return headers;
    }

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
                .<Payment, Payment>chunk(100, transactionManager)
                .reader(expiredPaymentItemReader())
                .processor(expiredPaymentItemProcessor())
                .writer(expiredPaymentItemWriter())
                .build();
    }

    @Bean
    public JpaCursorItemReader<Payment> expiredPaymentItemReader() {
        String jpqlQuery = "SELECT p FROM Payment p WHERE p.endDate < :endDate AND (p.status = :canceled OR p.status = :approved) ORDER BY p.id ASC";
        JpaCursorItemReader<Payment> reader = new JpaCursorItemReader<>();
        reader.setEntityManagerFactory(entityManagerFactory);
        reader.setQueryString(jpqlQuery);
        reader.setParameterValues(Map.of(
                "endDate", LocalDate.now(),
                "canceled", PaymentStatus.CANCELED,
                "approved", PaymentStatus.APPROVED
        ));
        return reader;
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
            }
            return payment;
        };
    }

    @Bean
    public ItemWriter<Payment> expiredPaymentItemWriter() {
        // JpaItemWriter 설정
        JpaItemWriter<Payment> jpaItemWriter = new JpaItemWriter<>();
        jpaItemWriter.setEntityManagerFactory(entityManagerFactory);

        // Kafka 이벤트 발행을 위한 커스텀 ItemWriter
        ItemWriter<Payment> kafkaItemWriter = payments -> {
            for (Payment payment : payments) {
                if (payment.getStatus() == PaymentStatus.CANCELED) {
                    ExpirationEvent event = new ExpirationEvent(payment.getMemberId());
                    kafkaTemplate.send("expiration-topic", payment.getMemberId().toString(), event);
                }
            }
        };

        // CANCELED 상태 삭제용 ItemWriter
        ItemWriter<Payment> deleteCanceledPaymentsWriter = payments -> {
            for (Payment payment : payments) {
                if (payment.getStatus() == PaymentStatus.CANCELED) {
                    paymentRepository.delete(payment); // CANCELED 상태 삭제
                }
            }
        };

        // CompositeItemWriter로 세 가지 ItemWriter를 묶음
        CompositeItemWriter<Payment> compositeItemWriter = new CompositeItemWriter<>();
        compositeItemWriter.setDelegates(Arrays.asList(jpaItemWriter, kafkaItemWriter, deleteCanceledPaymentsWriter));

        return compositeItemWriter;
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

            kakaoWebClient.post()
                    .uri("https://open-api.kakaopay.com/online/v1/payment/subscription")
                    .headers(headers -> headers.addAll(getHeaders()))
                    .bodyValue(parameters)
                    .retrieve()
                    .toBodilessEntity()
                    .block();

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