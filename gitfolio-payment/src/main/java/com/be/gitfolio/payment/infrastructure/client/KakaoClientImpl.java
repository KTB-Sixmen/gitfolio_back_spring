package com.be.gitfolio.payment.infrastructure.client;

import com.be.gitfolio.payment.service.port.KakaoClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.Map;

@Component
public class KakaoClientImpl implements KakaoClient {

    private final WebClient webClient;

    public KakaoClientImpl(WebClient.Builder builder, @Value("${kakao.api.url}") String baseUrl) {
        this.webClient = builder.baseUrl(baseUrl).build();
    }

    private HttpHeaders getHeaders(String secretKey) {
        HttpHeaders headers = new HttpHeaders();
        String auth = "SECRET_KEY " + secretKey;
        headers.set("Authorization", auth);
        headers.set("Content-Type", "application/json");
        return headers;
    }

    @Override
    public <T> T sendKakaoPaymentRequest(String url, Map<String, ?> parameters, Class<T> responseType, String secretKey) {
        return webClient.post()
                .uri(url)
                .headers(headers -> headers.addAll(getHeaders(secretKey)))
                .bodyValue(parameters)
                .retrieve()
                .bodyToMono(responseType)
                .block(); // 비동기 방식 사용 시 block() 제거 가능
    }
}
