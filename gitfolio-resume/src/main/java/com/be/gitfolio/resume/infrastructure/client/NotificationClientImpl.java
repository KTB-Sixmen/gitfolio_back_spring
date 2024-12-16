package com.be.gitfolio.resume.infrastructure.client;

import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.resume.service.port.NotificationClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.Disposable;

@Component
@Slf4j
public class NotificationClientImpl implements NotificationClient {

    private final WebClient webClient;

    public NotificationClientImpl(WebClient.Builder builder, @Value("${notification.server.url}") String baseUrl) {
        this.webClient = builder.baseUrl(baseUrl).build();
    }


    @Override
    public Disposable sendNotification(KafkaEvent.ResumeEvent resumeEvent) {
        return webClient.post()
                .uri("/api/notifications")
                .bodyValue(resumeEvent)
                .retrieve()
                .toBodilessEntity()
                .doOnSuccess(response -> log.info("Notification 전송 성공"))
                .doOnError(error -> log.error("Notification 전송 실패", error))
                .subscribe(); // 비동기로 실행;
    }
}
