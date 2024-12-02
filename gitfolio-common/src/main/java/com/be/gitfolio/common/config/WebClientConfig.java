package com.be.gitfolio.common.config;

import io.netty.channel.ChannelOption;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;

import java.time.Duration;

@Configuration
public class WebClientConfig {

    @Value("${member.server.url}")
    private String memberServiceBaseUrl;

    @Value("${ai.server.url}")
    private String aiServiceBaseUrl;

    @Value("${notification.server.url}")
    private String notificationServiceBaseUrl;

    @Value("${github.api.url}")
    private String githubBaseUrl;

    @Value("${kakao.api.url}")
    private String kakaoBaseUrl;

    @Bean
    public WebClient memberWebClient(WebClient.Builder builder) {
        return builder
                .baseUrl(memberServiceBaseUrl)
                .build();
    }

    @Bean
    public WebClient aiWebClient(WebClient.Builder builder) {
        return builder
                .baseUrl(aiServiceBaseUrl)
                .clientConnector(new ReactorClientHttpConnector(
                        HttpClient.create()
                                .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, 20000) // 연결 타임아웃 20초
                                .responseTimeout(Duration.ofMinutes(5)) // 응답 대기 타임아웃 5분
                ))
                .build();
    }

    @Bean
    public WebClient notificationWebClient(WebClient.Builder builder) {
        return builder
                .baseUrl(notificationServiceBaseUrl)
                .build();
    }

    @Bean
    public WebClient githubWebClient(WebClient.Builder builder) {
        return builder
                .baseUrl(githubBaseUrl)
                .build();
    }

    @Bean
    public WebClient kakaoWebClient(WebClient.Builder builder) {
        return builder
                .baseUrl(kakaoBaseUrl)
                .build();
    }

}
