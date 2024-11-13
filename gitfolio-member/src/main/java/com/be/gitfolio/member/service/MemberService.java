package com.be.gitfolio.member.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import com.be.gitfolio.member.repository.MemberAdditionalInfoRepository;
import com.be.gitfolio.member.repository.MemberRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.reactive.function.client.WebClient;

import java.io.IOException;
import java.time.Instant;
import java.util.*;

import static com.be.gitfolio.member.dto.MemberRequestDTO.*;
import static com.be.gitfolio.member.dto.MemberResponseDTO.*;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class MemberService {

    @Value(("${github.api.token}"))
    private String GITHUB_API_TOKEN;


    private final MemberRepository memberRepository;
    private final MemberAdditionalInfoRepository memberAdditionalInfoRepository;
    private final S3Service s3Service;
    private final WebClient githubWebClient;

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
     * member 정보 가져오기 (내부통신용) 이미지 prefix없이 보내는 용도
     */
    public MemberDetailDTO sendMemberDetailToResume(Long memberId) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        // MongoDB에서 추가 정보 조회
        Optional<MemberAdditionalInfo> additionalInfoOpt = memberAdditionalInfoRepository.findByMemberId(member.getId().toString());

        // 추가 정보가 없으면 기본값으로 처리
        MemberAdditionalInfo additionalInfo = additionalInfoOpt
                .orElseGet(() -> MemberAdditionalInfo.from(member.getId()));

        // DTO에 MySQL과 MongoDB 데이터를 함께 담아 반환
        return MemberDetailDTO.of(member, additionalInfo, member.getAvatarUrl());
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

        // s3에 저장된 파일이면 prefix 붙여서 제공
        String avatarUrl = member.getAvatarUrl();
        if (!avatarUrl.contains("avatars.githubusercontent.com")) {
            avatarUrl = s3Service.getFullFileUrl(avatarUrl);
        }

        // DTO에 MySQL과 MongoDB 데이터를 함께 담아 반환
        return MemberDetailDTO.of(member, additionalInfo, avatarUrl);
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
     * 회원 레포 조회
     */
    public List<MemberGithubRepositoryDTO> getUserRepositoriesWithLanguages(String username) {

        // 1. 사용자 개인 레포지토리 조회
        List<MemberGithubRepositoryDTO> userRepositories = new ArrayList<>(getRepositoriesForUser(username));

        // 2. 사용자가 속한 조직 리스트 조회
        List<String> organizations = getOrganizationsForUser(username);

        // 3. 각 조직의 레포지토리 조회 및 추가
        for (String org : organizations) {
            userRepositories.addAll(getRepositoriesForOrganization(org));
        }

        // 4. updatedAt 기준으로 정렬
        userRepositories.sort(Comparator.comparing(
                repo -> Instant.parse(repo.updatedAt()),
                Comparator.reverseOrder()
        ));


        return userRepositories;
    }

    /**
     * 회원 플랜 변경
     */
    @Transactional
    public Void updateMemeberPlan(Long memberId, PaidPlan paidPlan) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        member.updatePlan(paidPlan);
        return null;
    }

    /**
     * 회원 탈퇴
     */
    @Transactional
    public void deleteMember(Long memberId) {
        memberRepository.deleteById(memberId);
        memberAdditionalInfoRepository.deleteByMemberId(String.valueOf(memberId));
    }

    // 사용자 개인 레포지토리를 조회하는 메서드
    private List<MemberGithubRepositoryDTO> getRepositoriesForUser(String username) {
        return githubWebClient.get()
                .uri("/users/{username}/repos", username)
                .header("Authorization", "Bearer " + GITHUB_API_TOKEN)
                .retrieve()
                .bodyToFlux(Map.class)
                .map(MemberGithubRepositoryDTO::from)
                .collectList()
                .block();
    }

    // 사용자가 속한 조직 리스트를 조회하는 메서드
    private List<String> getOrganizationsForUser(String username) {
        return githubWebClient.get()
                .uri("/users/{username}/orgs", username)
                .header("Authorization", "Bearer " + GITHUB_API_TOKEN)
                .retrieve()
                .bodyToFlux(Map.class)
                .map(org -> (String) org.get("login"))  // 조직 이름(login)을 추출
                .collectList()
                .block();
    }

    // 조직의 레포지토리 목록을 조회하는 메서드
    private List<MemberGithubRepositoryDTO> getRepositoriesForOrganization(String org) {
        return githubWebClient.get()
                .uri("/orgs/{org}/repos", org)
                .header("Authorization", "Bearer " + GITHUB_API_TOKEN)
                .retrieve()
                .bodyToFlux(Map.class)
                .map(MemberGithubRepositoryDTO::from)
                .collectList()
                .block();
    }
}