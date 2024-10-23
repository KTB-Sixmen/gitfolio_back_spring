package com.be.gitfolio.member.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import com.be.gitfolio.member.repository.MemberAdditionalInfoRepository;
import com.be.gitfolio.member.repository.MemberRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static com.be.gitfolio.member.dto.MemberRequestDTO.*;
import static com.be.gitfolio.member.dto.MemberResponseDTO.*;

@Service
@Slf4j
@Transactional(readOnly = true)
public class MemberService {

    @Value(("${github.api.token}"))
    private String GITHUB_API_TOKEN;


    private final MemberRepository memberRepository;
    private final MemberAdditionalInfoRepository memberAdditionalInfoRepository;
    private final S3Service s3Service;
    private final WebClient webClient;

    public MemberService(MemberRepository memberRepository,
                         MemberAdditionalInfoRepository memberAdditionalInfoRepository,
                         S3Service s3Service,
                         @Value("${github.api.url}") String url, WebClient.Builder webClientBuilder) {
        this.memberRepository = memberRepository;
        this.memberAdditionalInfoRepository = memberAdditionalInfoRepository;
        this.s3Service = s3Service;
        this.webClient = webClientBuilder.baseUrl(url).build();
    }

    /**
     * 회원 생성
     */
    @Transactional
    public Long createMember(MemberCreateRequestDTO memberCreateRequestDTO) {
        // 회원 기본정보 생성 및 저장
        Member member = Member.from(memberCreateRequestDTO);
        Member savedMember = memberRepository.save(member);

        // 회원 추가정보 생성만
        MemberAdditionalInfo memberAdditionalInfo = MemberAdditionalInfo.from(savedMember.getId());
        memberAdditionalInfoRepository.save(memberAdditionalInfo);

        return savedMember.getId();
    }

    /**
     * username으로 회원 ID(PK) 조회
     */
    public Member findMemberIdByUsername(String username) {
        return memberRepository.findByUsername(username);
    }


    /**
     * 회원 정보 상세 조회
     */
    public MemberDetailDTO getMemberDetails(Long memberId) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        // MongoDB에서 추가 정보 조회
        Optional<MemberAdditionalInfo> additionalInfoOpt = memberAdditionalInfoRepository.findByMemberId(member.getId().toString());

        // 추가 정보가 없으면 기본값으로 처리
        MemberAdditionalInfo additionalInfo = additionalInfoOpt
                .orElseGet(() -> MemberAdditionalInfo.from(member.getId()));

        // DTO에 MySQL과 MongoDB 데이터를 함께 담아 반환
        return MemberDetailDTO.of(member, additionalInfo);
    }

    /**
     * 회원 기본 정보 수정
     */
    @Transactional
    public void updateMemberInfo(Long memberId,
                                 MemberUpdateRequestDTO memberUpdateRequestDTO,
                                 MemberAdditionalRequestDTO memberAdditionalRequestDTO,
                                 MultipartFile imageFile) throws IOException {
        // 기본 정보 수정
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        String avatarUrl = member.getAvatarUrl();

        // 이미지 파일이 있을 경우에만 업로드 진행
        if (imageFile != null && !imageFile.isEmpty()) {
            // 기존 이미지가 깃허브 기본 이미지가 아니면 삭제
            if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
                s3Service.deleteFile(avatarUrl);
            }

            // 새 이미지 업로드
            avatarUrl = s3Service.uploadFile(imageFile);
        }

        member.updateMember(memberUpdateRequestDTO, avatarUrl);
        memberRepository.save(member);

        // 추가 정보 수정
        MemberAdditionalInfo memberAdditionalInfo = memberAdditionalInfoRepository.findByMemberId(memberId.toString())
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_ADDITIONAL_INFO));

        memberAdditionalInfo.updateMemberAdditionalInfo(memberAdditionalRequestDTO);
        memberAdditionalInfoRepository.save(memberAdditionalInfo);
    }

    /**
     * 레포지토리 목록과 각 레포지토리의 주 언어 정보를 가져오는 메서드
     */
    public List<MemberGithubRepositoryDTO> getUserRepositoriesWithLanguages(String username) {
        return webClient.get()
                .uri("/users/{username}/repos", username)
                .header("Authorization", "Bearer " + GITHUB_API_TOKEN)
                .retrieve()
                .bodyToFlux(Map.class)  // JSON 응답을 Map으로 받음
                .map(repo -> MemberGithubRepositoryDTO.builder()
                            .repoName((String) repo.get("name"))
                            .repoUrl((String) repo.get("html_url"))  // URL 필드는 "html_url"에 존재
                            .repoId(Long.valueOf((Integer) repo.get("id")))
                            .updatedAt((String) repo.get("updated_at"))
                            .topLanguage((String) repo.get("language"))
                            .build()
                )
                .collectList()
                .block();  // 블로킹 방식으로 List 반환
    }



}