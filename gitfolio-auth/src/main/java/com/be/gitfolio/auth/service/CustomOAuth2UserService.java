package com.be.gitfolio.auth.service;

import com.be.gitfolio.auth.dto.CustomOAuth2User;
import com.be.gitfolio.auth.dto.GithubResponse;
import com.be.gitfolio.auth.dto.MemberDTO;
import com.be.gitfolio.common.config.BaseResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import static com.be.gitfolio.auth.dto.MemberDTO.*;

@Service
@Slf4j
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final WebClient webClient;

    public CustomOAuth2UserService(WebClient.Builder webClientBuilder) {
        this.webClient = webClientBuilder.baseUrl("http://localhost:8081").build();
    }

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) throws OAuth2AuthenticationException {
        OAuth2User oAuth2User = super.loadUser(userRequest);
        log.info("OAuth2User: {}", oAuth2User);

        GithubResponse githubResponse = new GithubResponse(oAuth2User.getAttributes());
        String username = githubResponse.getGithubId();

        // 회원 조회 및 처리
        return findMemberIdByUsername(username)
                .switchIfEmpty(createMemberInMemberModule(createMemberSaveRequest(githubResponse)))
                .map(memberId -> createCustomOAuth2User(memberId, githubResponse))
                .block();  // Mono를 동기적으로 처리하여 OAuth2User 반환
    }

    // 회원 정보 조회를 위한 WebClient 호출 메서드
    private Mono<Long> findMemberIdByUsername(String username) {
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

    // 회원 가입을 위한 WebClient 호출 메서드
    private Mono<Long> createMemberInMemberModule(MemberSaveRequestDTO memberSaveRequestDTO) {
        return webClient.post()
                .uri("/api/members")
                .bodyValue(memberSaveRequestDTO)
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<BaseResponse<Long>>() {})
                .map(BaseResponse::getResult);
    }

    // 회원 저장 요청 DTO 생성
    private MemberSaveRequestDTO createMemberSaveRequest(GithubResponse githubResponse) {
        // GithubResponse 값 확인
        log.info("GitHub Response: Username={}, Name={}, AvatarUrl={}", githubResponse.getGithubId(), githubResponse.getName(), githubResponse.getAvatarUrl());
        return MemberSaveRequestDTO.builder()
                .username(githubResponse.getGithubId())
                .name(githubResponse.getName())
                .role("ROLE_USER")
                .avatarUrl(githubResponse.getAvatarUrl())
                .build();
    }

    // CustomOAuth2User 생성 메서드
    private CustomOAuth2User createCustomOAuth2User(Long memberId, GithubResponse githubResponse) {
        OAuth2UserDTO memberDTO = OAuth2UserDTO.builder()
                .memberId(memberId)
                .username(githubResponse.getGithubId())
                .name(githubResponse.getName())
                .role("ROLE_USER")
                .build();

        return new CustomOAuth2User(memberDTO);
    }
}