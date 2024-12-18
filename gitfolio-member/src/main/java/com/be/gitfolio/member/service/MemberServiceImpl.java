package com.be.gitfolio.member.service;

import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.member.controller.port.MemberService;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import com.be.gitfolio.member.domain.request.MemberAdditionalInfoRequest.MemberAdditionalInfoUpdate;
import com.be.gitfolio.member.event.MemberEventPublisher;
import com.be.gitfolio.member.service.port.GithubClient;
import com.be.gitfolio.member.service.port.MemberAdditionalInfoRepository;
import com.be.gitfolio.member.service.port.MemberRepository;
import lombok.Builder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.time.Instant;
import java.util.*;

import static com.be.gitfolio.common.event.KafkaEvent.*;
import static com.be.gitfolio.member.domain.request.MemberRequest.*;
import static com.be.gitfolio.member.controller.response.MemberResponse.*;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
@Builder
public class MemberServiceImpl implements MemberService {

    private final MemberRepository memberRepository;
    private final MemberAdditionalInfoRepository memberAdditionalInfoRepository;
    private final S3Service s3Service;
    private final GithubClient githubClient;
    private final JobLauncher jobLauncher;
    private final Job remainingCountResetJob;
    private final JobRepository jobRepository;
    private final MemberEventPublisher memberEventPublisher;

    /**
     * 회원 생성
     */
    @Transactional
    @Override
    public Member createMember(MemberCreate memberCreate) {
        // 회원 기본정보 생성 및 저장
        Member member = Member.from(memberCreate);
        Member savedMember = memberRepository.save(member);
        log.info("저장된 회원의 GithubName : {}", savedMember.getGithubName());

        // 회원 추가정보 생성만
        MemberAdditionalInfo memberAdditionalInfo = MemberAdditionalInfo.from(savedMember.getId());
        memberAdditionalInfoRepository.save(memberAdditionalInfo);

        return savedMember;
    }

    /**
     * username으로 회원 ID(PK) 조회
     */
    @Override
    public Optional<Member> findMemberIdByUsername(String username) {
        return memberRepository.findByUsername(username);
    }

    /**
     * member 정보 가져오기 (내부통신용) 이미지 prefix없이 보내는 용도
     */
    @Override
    public MemberDetail sendMemberDetailToResume(Long memberId) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        // MongoDB에서 추가 정보 조회
        Optional<MemberAdditionalInfo> additionalInfoOpt = memberAdditionalInfoRepository.findByMemberId(member.getId());

        // 추가 정보가 없으면 기본값으로 처리
        MemberAdditionalInfo additionalInfo = additionalInfoOpt
                .orElseGet(() -> MemberAdditionalInfo.from(member.getId()));

        // DTO에 MySQL과 MongoDB 데이터를 함께 담아 반환
        return MemberDetail.of(member, additionalInfo, member.getAvatarUrl());
    }

    /**
     * 회원 정보 상세 조회
     */
    @Override
    public MemberDetail getMemberDetails(Long memberId) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        // MongoDB에서 추가 정보 조회
        Optional<MemberAdditionalInfo> additionalInfoOpt = memberAdditionalInfoRepository.findByMemberId(member.getId());

        // 추가 정보가 없으면 기본값으로 처리
        MemberAdditionalInfo additionalInfo = additionalInfoOpt
                .orElseGet(() -> MemberAdditionalInfo.from(member.getId()));

        // s3에 저장된 파일이면 prefix 붙여서 제공
        String avatarUrl = member.getAvatarUrl();
        if (!avatarUrl.contains("avatars.githubusercontent.com")) {
            avatarUrl = s3Service.getFullFileUrl(avatarUrl);
        }

        // DTO에 MySQL과 MongoDB 데이터를 함께 담아 반환
        return MemberDetail.of(member, additionalInfo, avatarUrl);
    }

    /**
     * 회원 기본 정보 수정
     */
    @Transactional
    @Override
    public void updateMemberInfo(Long memberId,
                                 MemberUpdate memberUpdate,
                                 MemberAdditionalInfoUpdate memberAdditionalInfoUpdate,
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

        Member updateMember = member.updateMember(memberUpdate, avatarUrl);
        memberRepository.save(updateMember);

        // 추가 정보 수정
        MemberAdditionalInfo memberAdditionalInfo = memberAdditionalInfoRepository.findByMemberId(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_ADDITIONAL_INFO));

        MemberAdditionalInfo updateMemberAdditionalInfo = memberAdditionalInfo.updateMemberAdditionalInfo(memberAdditionalInfoUpdate);
        memberAdditionalInfoRepository.save(updateMemberAdditionalInfo);
    }

    /**
     * 회원 레포 조회
     */
    @Cacheable(value = "memberCache", key = "#username")
    @Override
    public List<MemberGithubRepository> getUserRepositoriesWithLanguages(String username) {

        // 1. 사용자 개인 레포지토리 조회
        List<MemberGithubRepository> userRepositories = new ArrayList<>(githubClient.getRepositoriesForUser(username));

        // 2. 사용자가 속한 조직 리스트 조회
        List<String> organizations = githubClient.getOrganizationsForUser(username);

        // 3. 각 조직의 레포지토리 조회 및 추가
        for (String org : organizations) {
            userRepositories.addAll(githubClient.getRepositoriesForOrganization(org));
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
    @Override
    public Void updateMemeberPlan(Long memberId, PaidPlan paidPlan) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        Member updatedMember = member.updatePlan(paidPlan);
        memberRepository.save(updatedMember);
        return null;
    }

    /**
     * 회원 탈퇴
     */
    @Transactional
    @Override
    public void deleteMember(Long memberId) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));
        memberRepository.deleteById(memberId);
        memberAdditionalInfoRepository.deleteByMemberId(memberId);
        memberEventPublisher.publishResumeEvent(memberId, member.getUsername());
    }

    /**
     * 회원 잔여 사용권 차감
     * @param memberId
     * @return
     */
    @Transactional
    @Override
    public Void decreaseRemainingCount(Long memberId) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        Member updatedMember = member.decreaseRemainingCount();
        memberRepository.save(updatedMember);
        return null;
    }

    /**
     * 플랜 만료 이벤트 핸들러
     */
    @KafkaListener(topics = "expiration-topic", groupId = "member-service-group")
    @Transactional
    public void handleExpirationEvent(ExpirationEvent event) {
        Long memberId = event.getMemberId();
        Optional<Member> memberOpt = memberRepository.findById(memberId);

        if (memberOpt.isPresent()) {
            Member member = memberOpt.get();
            Member updatedMember = member.updatePlan(PaidPlan.FREE);
            memberRepository.save(updatedMember);
        } else {
            log.info("존재하지 않는 사용자의 결제 만료");
        }
    }

    /**
     * payment 배치 작업 완료 이벤트 핸들러
     */
    @KafkaListener(topics = "payment-batch-complete-topic", groupId = "member-service-group")
    @Transactional(propagation = Propagation.NOT_SUPPORTED)
    public void handleBatchCompleteEvent(String message) {
        if ("BatchCompleted".equals(message)) {
            try {
                JobParameters parameters = new JobParametersBuilder()
                        .addLong("time", System.currentTimeMillis())
                        .toJobParameters();

                // 별도의 트랜잭션에서 실행
                runJobInNewTransaction(parameters);

            } catch (BaseException e) {
                log.error("Error during batch completion handling: {}", e.getMessage());
            }
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW) // 새 트랜잭션에서 실행
    public void runJobInNewTransaction(JobParameters parameters) {
        try {
            jobLauncher.run(remainingCountResetJob, parameters);
        } catch (JobInstanceAlreadyCompleteException e) {
            throw new BaseException(ErrorCode.ALREADY_COMPLETED_JOB);
        } catch (JobExecutionAlreadyRunningException e) {
            throw new BaseException(ErrorCode.ALREADY_RUNNING_JOB);
        } catch (JobParametersInvalidException e) {
            throw new BaseException(ErrorCode.INVALID_JOB_PARAMETER);
        } catch (JobRestartException e) {
            throw new BaseException(ErrorCode.CANNOT_RESTART_JOB);
        }
    }
}