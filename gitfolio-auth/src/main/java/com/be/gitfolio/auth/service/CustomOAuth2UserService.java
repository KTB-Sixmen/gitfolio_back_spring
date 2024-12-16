package com.be.gitfolio.auth.service;

import com.be.gitfolio.auth.dto.CustomOAuth2User;
import com.be.gitfolio.auth.dto.GithubResponse;
import com.be.gitfolio.auth.service.port.MemberClient;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import static com.be.gitfolio.auth.dto.MemberDTO.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final MemberClient memberClient;

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) throws OAuth2AuthenticationException {
        OAuth2User oAuth2User = super.loadUser(userRequest);
        log.info("OAuth2User: {}", oAuth2User);

        GithubResponse githubResponse = new GithubResponse(oAuth2User.getAttributes());
        String username = githubResponse.getGithubId();

        // 회원 조회 및 신규 사용자 여부 판단
        Mono<Long> memberIdMono = findMemberIdByUsername(username);
        boolean isNewMember = memberIdMono.blockOptional().isEmpty();  // 회원 ID가 없으면 신규 사용자

        // 회원 조회 및 처리
        return memberIdMono.switchIfEmpty(createMemberInMemberModule(createMemberSaveRequest(githubResponse)))
                .map(memberId -> createCustomOAuth2User(memberId, githubResponse, isNewMember))
                .block();
    }

    // 회원 정보 조회를 위한 WebClient 호출 메서드
    private Mono<Long> findMemberIdByUsername(String username) {
        return memberClient.findMemberIdByUsername(username);
    }

    // 회원 가입을 위한 WebClient 호출 메서드
    private Mono<Long> createMemberInMemberModule(MemberSaveRequestDTO memberSaveRequestDTO) {
        return memberClient.createMemberInMemberModule(memberSaveRequestDTO);
    }

    // 회원 저장 요청 DTO 생성
    private MemberSaveRequestDTO createMemberSaveRequest(GithubResponse githubResponse) {
        // GithubResponse 값 확인
        log.info("GitHub Response: Username={}, Name={}, AvatarUrl={}", githubResponse.getGithubId(), githubResponse.getName(), githubResponse.getAvatarUrl());
        return MemberSaveRequestDTO.from(githubResponse);
    }

    // CustomOAuth2User 생성 메서드
    private CustomOAuth2User createCustomOAuth2User(Long memberId, GithubResponse githubResponse, boolean isNewMember) {
        OAuth2UserDTO memberDTO = new OAuth2UserDTO(
                memberId,
                "ROLE_USER",
                githubResponse.getName(),
                githubResponse.getGithubId()
        );

        return new CustomOAuth2User(memberDTO, isNewMember);
    }
}