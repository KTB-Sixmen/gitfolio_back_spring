package com.be.gitfolio.member.config;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.member.infrastructure.member.MemberEntity;
import jakarta.persistence.EntityManagerFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.database.BeanPropertyItemSqlParameterSourceProvider;
import org.springframework.batch.item.database.JdbcBatchItemWriter;
import org.springframework.batch.item.database.JdbcPagingItemReader;
import org.springframework.batch.item.database.support.SqlPagingQueryProviderFactoryBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.retry.backoff.FixedBackOffPolicy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.transaction.PlatformTransactionManager;

import javax.sql.DataSource;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

@Configuration
@EnableBatchProcessing
@RequiredArgsConstructor
public class MemberBatchConfig {

    private final JobRepository jobRepository;
    private final EntityManagerFactory entityManagerFactory;
    private final DataSource dataSource;

    @Bean
    public Job remainingCountResetJob(Step remainingCountResetStep) {
        return new JobBuilder("remainingCountResetJob", jobRepository)
                .start(remainingCountResetStep)
                .build();
    }

    @Bean
    public Step remainingCountResetStep(PlatformTransactionManager transactionManager) {
        return new StepBuilder("remainingCount", jobRepository)
                .<MemberEntity, MemberEntity>chunk(500, transactionManager)
                .reader(freeMemberItemReader())
                .processor(freeMemberItemProcessor())
                .writer(freeMemberItemWriter())
                .taskExecutor(taskExecutor())
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

    // JDBC 기반 PagingItemReader 설정
    @Bean
    public JdbcPagingItemReader<MemberEntity> freeMemberItemReader() {
        JdbcPagingItemReader<MemberEntity> reader = new JdbcPagingItemReader<>();
        reader.setDataSource(dataSource);
        reader.setFetchSize(500);
        reader.setRowMapper(new MemberRowMapper()); // 커스텀 RowMapper 설정

        Map<String, Object> parameterValues = new HashMap<>();
        parameterValues.put("paidPlan", PaidPlan.FREE.name());
        reader.setParameterValues(parameterValues);

        try {
            SqlPagingQueryProviderFactoryBean queryProvider = new SqlPagingQueryProviderFactoryBean();
            queryProvider.setDataSource(dataSource);
            queryProvider.setSelectClause("SELECT *");
            queryProvider.setFromClause("FROM member"); // 실제 테이블 이름 확인
            queryProvider.setWhereClause("WHERE paid_plan = :paidPlan");
            queryProvider.setSortKey("member_id"); // 고유한 정렬 키 사용 (테이블의 PK)
            reader.setQueryProvider(queryProvider.getObject());
        } catch (Exception e) {
            throw new RuntimeException("Failed to create query provider for JdbcPagingItemReader", e);
        }

        return reader;
    }

    // 커스텀 RowMapper 구현
    public static class MemberRowMapper implements RowMapper<MemberEntity> {
        @Override
        public MemberEntity mapRow(ResultSet rs, int rowNum) throws SQLException {
            return MemberEntity.builder()
                    .id(rs.getLong("member_id")) // @Column(name = "member_id")
                    .username(rs.getString("username"))
                    .nickname(rs.getString("nickname"))
                    .name(rs.getString("name"))
                    .githubName(rs.getString("github_name"))
                    .role(rs.getString("role"))
                    .avatarUrl(rs.getString("avatar_url"))
                    .phoneNumber(rs.getString("phone_number"))
                    .email(rs.getString("email"))
                    .position(PositionType.valueOf(rs.getString("position"))) // Enum 매핑
                    .paidPlan(PaidPlan.valueOf(rs.getString("paid_plan"))) // Enum 매핑
                    .remainingCount(rs.getInt("remaining_count"))
                    .createdAt(rs.getTimestamp("created_at").toLocalDateTime())
                    .updatedAt(rs.getTimestamp("updated_at").toLocalDateTime())
                    .build();
        }
    }

    @Bean
    public ItemProcessor<MemberEntity, MemberEntity> freeMemberItemProcessor() {
        return memberEntity -> {
            // 잔여 카운트 3으로 변경
            memberEntity.resetCount();
            return memberEntity;
        };
    }

    // JDBC 기반 Writer 설정: 잔여 카운트 업데이트
    @Bean
    public JdbcBatchItemWriter<MemberEntity> freeMemberItemWriter() {
        JdbcBatchItemWriter<MemberEntity> writer = new JdbcBatchItemWriter<>();
        writer.setDataSource(dataSource);
        writer.setSql("UPDATE member SET remaining_count = :remainingCount WHERE member_id = :id");
        writer.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
        writer.setAssertUpdates(false); // 업데이트된 행이 없더라도 예외를 발생시키지 않음

        return writer;
    }
}
