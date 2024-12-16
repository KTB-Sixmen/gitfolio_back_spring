package com.be.gitfolio.auth.infrastructure.client;

import com.be.gitfolio.auth.dto.MemberDTO;
import com.be.gitfolio.auth.service.port.MemberClient;
import com.be.gitfolio.common.config.BaseResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;


@Component
public class MemberClientImpl implements MemberClient {

    private final WebClient webClient;

    public MemberClientImpl(WebClient.Builder builder, @Value("${member.server.url}") String baseUrl) {
        this.webClient = builder.baseUrl(baseUrl).build();
    }

    @Override
    public Mono<Long> findMemberIdByUsername(String username) {
        return webClient.get()
                .uri(uriBuilder -> uriBuilder
                        .path("/api/members")
                        .queryParam("username", username)
                        .build())
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<BaseResponse<Long>>() {})
                .flatMap(baseResponse -> {
                    Long result = baseResponse.getResult();
                    if (result == null) {
                        return Mono.empty(); // 회원이 없을 경우 빈 값 반환
                    }
                    return Mono.just(result); // 회원 ID가 존재할 경우 반환
                });
    }

    @Override
    public Mono<Long> createMemberInMemberModule(MemberDTO.MemberSaveRequestDTO memberSaveRequestDTO) {
        return webClient.post()
                .uri("/api/members")
                .bodyValue(memberSaveRequestDTO)
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<BaseResponse<Long>>() {})
                .map(BaseResponse::getResult);
    }
}
