package com.be.gitfolio.payment.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class PaymentJobCompletionListener implements JobExecutionListener {

    private final KafkaTemplate<String, String> kafkaTemplate;

    @Override
    public void afterJob(JobExecution jobExecution) {
        if (jobExecution.getStatus() == BatchStatus.COMPLETED) {
            // 배치작업 정상 수행 완료 시
            kafkaTemplate.send("payment-batch-complete-topic", "BatchCompleted");
            log.info("배치작업 수행 완료. 이벤트 발행 성공.");
        } else {
            log.info("배치작업 수행 실패");
        }
    }
}
