package com.be.gitfolio.payment.schedule;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Configuration
@EnableScheduling
@RequiredArgsConstructor
@Slf4j
public class PaymentBatchScheduler {

    private final JobLauncher jobLauncher;
    private final Job paymentExpirationJob;

    @Scheduled(cron = "0 0 0 * * *") // 매일 자정에 실행
    public void runPaymentExpirationJob() throws Exception {
        log.info("Payment Job Start");

        JobParameters parameters = new JobParametersBuilder()
                .addLong("time", System.currentTimeMillis())
                .toJobParameters();

        jobLauncher.run(paymentExpirationJob, parameters);
    }
}
