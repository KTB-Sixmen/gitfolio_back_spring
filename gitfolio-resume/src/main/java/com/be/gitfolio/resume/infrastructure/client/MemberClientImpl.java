package com.be.gitfolio.resume.infrastructure.client;

import com.be.gitfolio.resume.service.port.MemberClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.Disposable;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

@Component
@Slf4j
public class MemberClientImpl implements MemberClient {

    private final WebClient webClient;

    public MemberClientImpl(WebClient.Builder builder, @Value("${member.server.url}") String baseUrl) {
        this.webClient = builder.baseUrl(baseUrl).build();
    }


    @Override
    public MemberInfoDTO getMemberInfo(Long memberId) {
        return webClient.get()
                .uri("/api/members/{memberId}", memberId)
                .retrieve()
                .bodyToMono(MemberInfoDTO.class)
                .block();
    }

    @Override
    public Disposable decreaseRemainingCount(Long memberId) {
        return webClient.patch()
                .uri("/api/members/{memberId}/remainingCount/decrease", memberId)
                .retrieve()
                .bodyToMono(Void.class)
                .doOnError(e -> log.error("Failed to decrease usage for memberId {}: {}", memberId, e.getMessage()))
                .subscribe(); // 비동기 요청으로 실행
    }

}
