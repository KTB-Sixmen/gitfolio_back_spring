package com.be.gitfolio.common.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.WebClient;

@Configuration
public class WebClientConfig {

    @Value("${member.server.url}")
    private String memberServiceBaseUrl;

    @Value("${ai.server.url}")
    private String aiServiceBaseUrl;

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
