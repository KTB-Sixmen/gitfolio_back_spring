package com.be.gitfolio.member.infrastructure.client;

import com.be.gitfolio.member.controller.response.MemberResponse;
import com.be.gitfolio.member.service.port.GithubClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.List;
import java.util.Map;

import static com.be.gitfolio.member.controller.response.MemberResponse.*;

@Component
public class GithubClientImpl implements GithubClient {

    @Value(("${github.api.token}"))
    private String GH_API_TOKEN;
    private final WebClient webClient;

    public GithubClientImpl(WebClient.Builder builder, @Value("${github.api.url}") String baseUrl) {
        this.webClient = builder.baseUrl(baseUrl).build();
    }

    // 사용자 개인 레포지토리를 조회하는 메서드
    @Override
    public List<MemberGithubRepository> getRepositoriesForUser(String username) {
        return webClient.get()
                .uri("/users/{username}/repos", username)
                .header("Authorization", "Bearer " + GH_API_TOKEN)
                .retrieve()
                .bodyToFlux(Map.class)
                .map(MemberGithubRepository::from)
                .collectList()
                .block();
    }

    // 사용자가 속한 조직 리스트를 조회하는 메서드
    @Override
    public List<String> getOrganizationsForUser(String username) {
        return webClient.get()
                .uri("/users/{username}/orgs", username)
                .header("Authorization", "Bearer " + GH_API_TOKEN)
                .retrieve()
                .bodyToFlux(Map.class)
                .map(org -> (String) org.get("login"))  // 조직 이름(login)을 추출
                .collectList()
                .block();
    }

    // 조직의 레포지토리 목록을 조회하는 메서드
    @Override
    public List<MemberGithubRepository> getRepositoriesForOrganization(String org) {
        return webClient.get()
                .uri("/orgs/{org}/repos", org)
                .header("Authorization", "Bearer " + GH_API_TOKEN)
                .retrieve()
                .bodyToFlux(Map.class)
                .map(MemberGithubRepository::from)
                .collectList()
                .block();
    }
}
