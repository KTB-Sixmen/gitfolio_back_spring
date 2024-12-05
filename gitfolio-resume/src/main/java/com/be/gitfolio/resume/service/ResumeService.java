package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.jwt.JWTUtil;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.Visibility;
import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.repository.comment.CommentJpaRepository;
import com.be.gitfolio.resume.service.port.LikeRepository;
import com.be.gitfolio.resume.service.port.ResumeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.logging.log4j.util.InternalException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class ResumeService {

    private final ResumeRepository resumeRepository;
    private final LikeRepository likeRepository;
    private final CommentJpaRepository commentJpaRepository;
    private final RedisTemplate<String, String> redisTemplate;
    private final WebClient aiWebClient;
    private final WebClient memberWebClient;
    private final JWTUtil jwtUtil;
    private final S3Service s3Service;


    /**
     * 이력서 생성 요청
     */
    @Transactional
    public String createResume(String memberId, CreateResumeRequestDTO createResumeRequestDTO) {

        MemberInfoDTO memberInfoDTO;
        try {
            memberInfoDTO = memberWebClient.get()
                    .uri("/api/members/{memberId}", Long.valueOf(memberId))
                    .retrieve()
                    .bodyToMono(MemberInfoDTO.class)
                    .block();
        } catch (WebClientResponseException exception) {
            throw new BaseException(ErrorCode.MEMBER_SERVER_ERROR);
        }

        // 사용권 없으면 예외
        if (memberInfoDTO.paidPlan().equals(PaidPlan.FREE) && memberInfoDTO.remainingCount() <= 0) {
            throw new BaseException(ErrorCode.REMAINING_COUNT_EXCEEDED);
        }

        String personalRepo = "https://github.com/" + memberInfoDTO.nickname();

        // AI에 요청할 DTO 생성
        AIRequestDTO aiRequestDTO = AIRequestDTO.of(memberInfoDTO, personalRepo, createResumeRequestDTO);

        AIResponseDTO aiResponseDTO;
        try {
            aiResponseDTO = aiWebClient.post()
                    .uri("/api/resumes")
                    .bodyValue(aiRequestDTO)
                    .retrieve()
                    .bodyToMono(AIResponseDTO.class)
                    .block();
        } catch (WebClientResponseException exception) {
            throw new InternalException("AIWebClient error" + exception.getMessage());
        }

        Resume resume = Resume.of(memberInfoDTO, aiResponseDTO, createResumeRequestDTO);
        String resumeId = resumeRepository.save(resume).getId();

        // FREE 사용자면 사용권 차감
        if (memberInfoDTO.paidPlan().equals(PaidPlan.FREE)) {
            memberWebClient.patch()
                    .uri("/api/members/{memberId}/remainingCount/decrease", Long.valueOf(memberId))
                    .retrieve()
                    .bodyToMono(Void.class)
                    .doOnError(e -> log.error("Failed to decrease usage for memberId {}: {}", memberId, e.getMessage()))
                    .subscribe(); // 비동기 요청으로 실행
        }

        return resumeId;
    }

    /**
     * 이력서 수정 (AI로)
     */
    @Transactional
    public ResumeInfoForAiDTO updateResumeWithAI(String memberId, String resumeId, UpdateResumeWithAIRequestDTO updateResumeWithAIRequestDTO) {

        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        // 이력서 생성한 사람이 본인인지 확인
        if (!resume.getMemberId().equals(memberId)) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_UPDATE_RESUME);
        }

        MemberInfoDTO memberInfoDTO;
        try {
            memberInfoDTO = memberWebClient.get()
                    .uri("/api/members/{memberId}", Long.valueOf(memberId))
                    .retrieve()
                    .bodyToMono(MemberInfoDTO.class)
                    .block();
        } catch (WebClientResponseException exception) {
            throw new BaseException(ErrorCode.MEMBER_SERVER_ERROR);
        }

        // 사용권 없으면 예외
        if (memberInfoDTO.paidPlan().equals(PaidPlan.FREE) && memberInfoDTO.remainingCount() <= 0) {
            throw new BaseException(ErrorCode.REMAINING_COUNT_EXCEEDED);
        }

        ResumeInfoForAiDTO resumeInfo = ResumeInfoForAiDTO.from(resume);
        log.info("resumeInfo : {}", resumeInfo);

        AIUpdateRequestDTO aiUpdateRequestDTO = AIUpdateRequestDTO.of(updateResumeWithAIRequestDTO, resumeInfo);

        ResumeInfoForAiDTO updateResumeInfo;
        try {
            updateResumeInfo = aiWebClient.put()
                    .uri("/api/resumes")
                    .bodyValue(aiUpdateRequestDTO)
                    .retrieve()
                    .bodyToMono(ResumeInfoForAiDTO.class)
                    .block();
        } catch (WebClientResponseException exception) {
            throw new InternalException("AIWebClient error" + exception.getMessage());
        }

        // FREE 사용자면 사용권 차감
        if (memberInfoDTO.paidPlan().equals(PaidPlan.FREE)) {
            memberWebClient.patch()
                    .uri("/api/members/{memberId}/remainingCount/decrease", Long.valueOf(memberId))
                    .retrieve()
                    .bodyToMono(Void.class)
                    .doOnError(e -> log.error("Failed to decrease usage for memberId {}: {}", memberId, e.getMessage()))
                    .subscribe(); // 비동기 요청으로 실행
        }

        return updateResumeInfo;
    }


    /**
     * 이력서 목록 조회
     */
    public PaginationResponseDTO<ResumeListDTO> getResumeList(String token, ResumeFilterDTO resumeFilterDTO) {
        // Token에서 사용자 ID 추출
        Long memberId = extractMemberIdFromToken(token);
        Pageable pageable = PageRequest.of(resumeFilterDTO.page(), resumeFilterDTO.size());

        // 1. 좋아요 필터가 있는 경우, MySQL에서 좋아요 누른 이력서 ID 목록 조회
        List<String> likedResumeIds = null;
        if (Boolean.TRUE.equals(resumeFilterDTO.liked())) {
            likedResumeIds = likeRepository.findLikedResumeIdsByMemberId(memberId);
            if (likedResumeIds.isEmpty()) {
                return new PaginationResponseDTO<>(List.of(), 0, pageable);
            }
        }

        // 2. MongoDB에서 이력서 목록 조회
        Page<Resume> resumePage = resumeRepository.findResumeByFilter(resumeFilterDTO, likedResumeIds, pageable);

        // 3. 좋아요 상태를 Map으로 변환 (Hard Delete 기반)
        Set<String> likedResumeSet = new HashSet<>(likeRepository.findLikedResumeIdsByMemberIdAndResumeIds(
                memberId, resumePage.stream().map(Resume::getId).toList()));

        // 4. 이력서 목록을 DTO로 변환
        List<ResumeListDTO> resumeListDTOs = resumePage.stream()
                .map(resume -> {
                    // Avatar URL 가공
                    String avatarUrl = resume.getAvatarUrl();
                    if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
                        avatarUrl = s3Service.getFullFileUrl(avatarUrl);
                    }

                    // 좋아요 여부 확인
                    boolean isLiked = likedResumeSet.contains(resume.getId());
                    return new ResumeListDTO(resume, isLiked, avatarUrl);
                })
                .toList();

        // 5. 결과 반환
        return new PaginationResponseDTO<>(resumeListDTOs, resumePage.getTotalElements(), pageable);
    }

    // Optional한 memberId 처리 함수
    private Long extractMemberIdFromToken(String token) {
        if (token == null || token.isEmpty()) {
            return null;
        }
        try {
            String accessToken = token.substring(7);
            return jwtUtil.getMemberId(accessToken);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * 이력서 상세 조회(커뮤니티)
     */
    @Transactional
    public ResumeDetailDTO getCommunityResumeDetail(String token, String resumeId, String clientIp) {
        // Token에서 사용자 ID 추출
        Long memberId = extractMemberIdFromToken(token);

        // 1. 이력서 조회
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        // private 이력서인 경우 예외처리
        if (resume.getVisibility().equals(Visibility.PRIVATE)) {
            throw new BaseException(ErrorCode.RESUME_ACCESS_DENIED);
        }

        // 2. 조회수 증가 및 저장
        incrementViewCount(resume, clientIp);
        resumeRepository.save(resume);

        // 3. 좋아요 여부 확인
        boolean isLiked = likeRepository.existsByResumeIdAndMemberId(resume.getId(), memberId);

        // 4. Avatar URL 가공
        String avatarUrl = resume.getAvatarUrl();
        if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
            avatarUrl = s3Service.getFullFileUrl(avatarUrl);
        }

        // 5. DTO 생성 및 반환
        return new ResumeDetailDTO(resume, isLiked, avatarUrl);
    }

    /**
     * 내 이력서 상세 조회(내 이력서)
     */
    @Transactional
    public ResumeDetailDTO getMyResumeDetail(String token, String resumeId, String clientIp) {
        // Token에서 사용자 ID 추출
        Long memberId = extractMemberIdFromToken(token);

        // 1. 이력서 조회
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        // 내 이력서가 아닌 경우 예외처리
        if (!resume.getMemberId().equals(String.valueOf(memberId))) {
            throw new BaseException(ErrorCode.RESUME_ACCESS_DENIED);
        }

        // 2. 조회수 증가 및 저장
        incrementViewCount(resume, clientIp);
        resumeRepository.save(resume);

        // 3. 좋아요 여부 확인
        boolean isLiked = likeRepository.existsByResumeIdAndMemberId(resume.getId(), memberId);

        // 4. Avatar URL 가공
        String avatarUrl = resume.getAvatarUrl();
        if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
            avatarUrl = s3Service.getFullFileUrl(avatarUrl);
        }

        // 5. DTO 생성 및 반환
        return new ResumeDetailDTO(resume, isLiked, avatarUrl);
    }

    /**
     * 내 이력서 목록 조회
     */
    public PaginationResponseDTO<ResumeListDTO> getMyResumeList(Long memberId, int page, int size) {
        // Pageable 객체 생성 (페이지 번호는 0부터 시작)
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "updatedAt"));

        // 1. MySQL에서 사용자의 이력서 목록을 페이지네이션으로 조회
        Page<Resume> resumePage = resumeRepository.findAllByMemberId(String.valueOf(memberId), pageable);

        // 2. 사용자의 모든 이력서 ID를 추출
        List<String> resumeIds = resumePage.getContent().stream()
                .map(Resume::getId)
                .toList();

        // 3. MySQL에서 사용자가 좋아요를 누른 이력서를 한 번에 조회
        List<Like> likes = likeRepository.findLikesByMemberIdAndResumeIds(memberId, resumeIds);
        Set<String> likedResumeIds = likes.stream()
                .map(Like::getResumeId)
                .collect(Collectors.toSet());

        // 4. 이력서 목록을 DTO로 변환
        List<ResumeListDTO> resumeListDTOs = resumePage.getContent().stream()
                .map(resume -> {
                    // 좋아요 여부 확인
                    boolean isLiked = likedResumeIds.contains(resume.getId());

                    // Avatar URL 가공
                    String avatarUrl = resume.getAvatarUrl();
                    if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
                        avatarUrl = s3Service.getFullFileUrl(avatarUrl);
                    }

                    return new ResumeListDTO(resume, isLiked, avatarUrl);
                })
                .toList();

        // 5. PaginationResponseDTO를 사용해 결과 반환
        return new PaginationResponseDTO<>(resumeListDTOs, resumePage.getTotalElements(), pageable);
    }

    /**
     * 이력서 삭제
     */
    @Transactional
    public void deleteResume(String resumeId) {
        likeRepository.deleteLikesByResumeId(resumeId); // 해당 이력서의 좋아요 전부 삭제
        commentJpaRepository.deleteCommentsByResumeId(resumeId); // 해당 이력서의 댓글 전부 삭제
        resumeRepository.deleteById(resumeId); // 이력서 삭제
    }

    /**
     * 이력서 직접 수정
     */
    @Transactional
    public void updateResume(String memberId, String resumeId, UpdateResumeRequestDTO updateResumeRequestDTO, MultipartFile imageFile) throws IOException {
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        // 이력서 생성한 사람이 본인인지 확인
        if (!resume.getMemberId().equals(memberId)) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_UPDATE_RESUME);
        }

        String avatarUrl = resume.getAvatarUrl();

        if (imageFile != null && !imageFile.isEmpty()) {
            avatarUrl = s3Service.uploadFile(imageFile);
        }

        resume.updateResume(updateResumeRequestDTO, avatarUrl);
        resumeRepository.save(resume);
    }

    /**
     * 조회수 증가 로직
     */
    private void incrementViewCount(Resume resume, String clientIp) {
        String redisKey = "resume:view:" + resume.getId() + ":" + clientIp;

        if (Boolean.FALSE.equals(redisTemplate.hasKey(redisKey))) {
            resume.updateView(); // 조회수 증가

            // 1시간 동안 조회수 증가 안되도록 설정
            redisTemplate.opsForValue().set(redisKey, "1", 1, TimeUnit.HOURS);
        }
    }

    /**
     * 이력서 공개 여부 변경
     * @param memberId
     * @param resumeId
     * @param updateVisibilityDTO
     */
    @Transactional
    public void updateVisibility(Long memberId, String resumeId, UpdateVisibilityDTO updateVisibilityDTO) {
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        if (!Objects.equals(resume.getMemberId(), String.valueOf(memberId))) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_UPDATE_RESUME);
        }

        resume.updateVisibility(updateVisibilityDTO.visibility());
        resumeRepository.save(resume);
    }
}
