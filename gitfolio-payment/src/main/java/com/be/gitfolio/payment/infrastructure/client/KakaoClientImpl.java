package com.be.gitfolio.payment.infrastructure.client;

import com.be.gitfolio.payment.service.port.KakaoClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.Map;

@Component
@Slf4j
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
        try {
            log.info("KakaoPay API Request - URL: {}, Parameters: {}, SecretKey: {}", url, parameters, secretKey);

            return webClient.post()
                    .uri(url)
                    .headers(headers -> headers.addAll(getHeaders(secretKey)))
                    .bodyValue(parameters)
                    .retrieve()
                    .bodyToMono(responseType)
                    .doOnNext(resp -> log.info("KakaoPay API Response - URL: {}, Response: {}", url, resp))
                    .block();
        } catch (WebClientResponseException e) {
            // HTTP 4xx, 5xx 상태 코드를 처리
            log.error("KakaoPay API Error - URL: {}, StatusCode: {}, ResponseBody: {}", url, e.getStatusCode(), e.getResponseBodyAsString(), e);
            throw e;
        } catch (Exception e) {
            // 기타 예외 처리
            log.error("KakaoPay API Unknown Error - URL: {}, Parameters: {}", url, parameters, e);
            throw e;
        }
    }
}
