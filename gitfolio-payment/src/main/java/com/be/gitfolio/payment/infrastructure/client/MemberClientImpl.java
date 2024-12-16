package com.be.gitfolio.payment.infrastructure.client;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.payment.service.port.MemberClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;

import static com.be.gitfolio.payment.dto.KakaoResponse.*;


@Component
public class MemberClientImpl implements MemberClient {

    private final WebClient webClient;

    public MemberClientImpl(WebClient.Builder builder, @Value("${member.server.url}") String baseUrl) {
        this.webClient = builder.baseUrl(baseUrl).build();
    }

    @Override
    public ResponseEntity<Void> updateMemberPaidPlan(Long memberId, KakaoApproveResponse kakaoApproveResponse) {
        return webClient.patch()
                .uri("/api/members/{memberId}/updatePlan", memberId)
                .bodyValue(PaidPlan.fromPlanName(kakaoApproveResponse.item_name()))
                .retrieve()
                .toBodilessEntity()
                .block();
    }
}
