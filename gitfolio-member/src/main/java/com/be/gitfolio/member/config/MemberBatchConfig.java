package com.be.gitfolio.member.config;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.member.infrastructure.member.MemberEntity;
import com.be.gitfolio.member.infrastructure.member.MemberJpaRepository;
import jakarta.persistence.EntityManagerFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.data.RepositoryItemWriter;
import org.springframework.batch.item.data.builder.RepositoryItemWriterBuilder;
import org.springframework.batch.item.database.JpaCursorItemReader;
import org.springframework.batch.item.database.builder.JpaCursorItemReaderBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import java.util.Map;

@Configuration
@EnableBatchProcessing
@RequiredArgsConstructor
public class MemberBatchConfig {

    private final JobRepository jobRepository;
    private final EntityManagerFactory entityManagerFactory;
    private final MemberJpaRepository memberRepository;

    @Bean
    public Job remainingCountResetJob(Step remainingCountResetStep) {
        return new JobBuilder("remainingCountResetJob", jobRepository)
                .start(remainingCountResetStep)
                .build();
    }

    @Bean
    public Step remainingCountResetStep(PlatformTransactionManager transactionManager) {
        return new StepBuilder("remainingCount", jobRepository)
                .<MemberEntity, MemberEntity>chunk(100, transactionManager)
                .reader(freeMemberItemReader())
                .processor(freeMemberItemProcessor())
                .writer(freeMemberItemWriter())
                .build();
    }

    @Bean
    public JpaCursorItemReader<MemberEntity> freeMemberItemReader() {
        return new JpaCursorItemReaderBuilder<MemberEntity>()
                .name("freeMemberItemReader")
                .entityManagerFactory(entityManagerFactory)
                .queryString("SELECT m FROM MemberEntity m WHERE m.paidPlan = :paidPlan ORDER BY m.id ASC")
                .parameterValues(Map.of("paidPlan", PaidPlan.FREE))
                .build();
    }

    @Bean
    public ItemProcessor<MemberEntity, MemberEntity> freeMemberItemProcessor() {
        return memberEntity -> {
            // 잔여 카운트 3으로 변경
            memberEntity.resetCount();
            return memberEntity;
        };
    }

    @Bean
    public RepositoryItemWriter<MemberEntity> freeMemberItemWriter() {
        return new RepositoryItemWriterBuilder<MemberEntity>()
                .repository(memberRepository)
                .methodName("save")
                .build();
    }
}
