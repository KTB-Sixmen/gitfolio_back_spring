package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.client.MemberGrpcClient;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.jwt.JWTUtil;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.event.ResumeEventPublisher;
import com.be.gitfolio.resume.mapper.MemberInfoMapper;
import com.be.gitfolio.resume.mapper.MemberMapper;
import com.be.gitfolio.resume.repository.CommentRepository;
import com.be.gitfolio.resume.repository.LikeRepository;
import com.be.gitfolio.resume.repository.ResumeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.reactive.function.client.WebClient;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;
import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class ResumeService {

    private final MemberGrpcClient memberGrpcClient;
    private final ResumeRepository resumeRepository;
    private final LikeRepository likeRepository;
    private final CommentRepository commentRepository;
    private final RedisTemplate<String, String> redisTemplate;
    private final WebClient aiWebClient;
    private final JWTUtil jwtUtil;
    private final ResumeEventPublisher resumeEventPublisher;
    private final MemberInfoMapper memberInfoMapper;
    private final S3Service s3Service;

    /**
     * 이력서 생성 요청
     */
    @Transactional
    public String createResume(String memberId, CreateResumeRequestDTO createResumeRequestDTO) {

        // Member 가져와서 personalRepo 만들기
        MemberResponseById memberResponse = memberGrpcClient.getMember(memberId);
        String personalRepo = "https://github.com/" + memberResponse.getNickname();

        // proto -> MemberInfoDTO
        MemberInfoDTO memberInfoDTO = memberInfoMapper.toMemberInfoDTO(memberResponse);

        // AI에 요청할 DTO 생성
        AIRequestDTO aiRequestDTO = AIRequestDTO.of(memberResponse, personalRepo, createResumeRequestDTO);
        log.info("GithubName : {}", memberResponse.getGithubName());

        AIResponseDTO aiResponseDTO = aiWebClient.post()
                .uri("/api/resumes")
                .bodyValue(aiRequestDTO)
                .retrieve()
                .bodyToMono(AIResponseDTO.class)
                .block();

        Resume resume = Resume.of(memberInfoDTO, aiResponseDTO);
        return resumeRepository.save(resume).getId();
    }


    /**
     * 이력서 목록 조회
     */
    public PaginationResponseDTO<ResumeListDTO> getResumeList(String token, ResumeFilterDTO resumeFilterDTO) {
        Long memberId = extractMemberIdFromToken(token);

        Pageable pageable = PageRequest.of(resumeFilterDTO.page(), resumeFilterDTO.size());

        Page<ResumeListDTO> resumePage = resumeRepository.findResumeByFilter(resumeFilterDTO, pageable)
                .map(resume -> {
                    Optional<Like> likeOpt = likeRepository.findByResumeIdAndMemberId(resume.getId(), memberId);
                    boolean isLiked = likeOpt.isPresent() && likeOpt.get().getStatus().equals(Boolean.TRUE);

                    // Avatar URL 가공
                    String avatarUrl = resume.getAvatarUrl();
                    if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
                        avatarUrl = s3Service.getFullFileUrl(avatarUrl);
                    }

                    return new ResumeListDTO(resume, isLiked, avatarUrl);
                });

        return new PaginationResponseDTO<>(resumePage);
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
     * 이력서 상세 조회
     */
    @Transactional
    public ResumeDetailDTO getResumeDetail(String token, String resumeId,String clientIp) {
        Long memberId = extractMemberIdFromToken(token);

        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));
        incrementViewCount(resume, clientIp);
        resumeRepository.save(resume);

        Optional<Like> likeOpt = likeRepository.findByResumeIdAndMemberId(resume.getId(), memberId);
        boolean isLiked = likeOpt.isPresent() && likeOpt.get().getStatus().equals(Boolean.TRUE);

        // Avatar URL 가공
        String avatarUrl = resume.getAvatarUrl();
        if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
            avatarUrl = s3Service.getFullFileUrl(avatarUrl);
        }

        return new ResumeDetailDTO(resume, isLiked, avatarUrl);
    }

    /**
     * 내 이력서 목록 조회
     */
    public List<ResumeListDTO> getMyResumeList(String memberId) {
        List<Resume> resumes = resumeRepository.findAllByMemberId(memberId, Sort.by(Sort.Direction.DESC, "updatedAt"));

        return resumes.stream()
                .map(resume -> {
                    Optional<Like> likeOpt = likeRepository.findByResumeIdAndMemberId(resume.getId(), Long.valueOf(memberId));
                    boolean isLiked = likeOpt.isPresent() && likeOpt.get().getStatus();

                    // Avatar URL 가공
                    String avatarUrl = resume.getAvatarUrl();
                    if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
                        avatarUrl = s3Service.getFullFileUrl(avatarUrl);
                    }

                    return new ResumeListDTO(resume, isLiked, avatarUrl);
                })
                .toList();
    }

    /**
     * 이력서 삭제
     */
    @Transactional
    public void deleteResume(String resumeId) {
        likeRepository.deleteLikesByResumeId(resumeId); // 해당 이력서의 좋아요 전부 삭제
        commentRepository.deleteCommentsByResumeId(resumeId); // 해당 이력서의 댓글 전부 삭제
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
            if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
                s3Service.deleteFile(avatarUrl);
            }

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
            log.info("New Viewer : {}", clientIp);
        }
    }

    /**
     * 좋아요 기능
     */
    @Transactional
    public boolean toggleLike(String resumeId, Long memberId) {
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        // 좋아요 상태를 확인하고, 상태에 따라 처리
        Optional<Like> existingLikeOpt = likeRepository.findByResumeIdAndMemberId(resumeId, memberId);
        boolean isLikeAdded = existingLikeOpt
                .map(existingLike -> {
                    existingLike.updateStatus();
                    if (Boolean.TRUE.equals(existingLike.getStatus())) {
                        resume.increaseLike();
                        // 이벤트 발행
                        resumeEventPublisher.publishResumeEvent(memberId, Long.valueOf(resume.getMemberId()), resumeId, NotificationType.LIKE);
                    } else {
                        resume.decreaseLike();
                    }
                    return false; // 좋아요 취소됨
                })
                .orElseGet(() -> {
                    Like newLike = Like.of(resumeId, memberId);
                    resume.increaseLike();
                    likeRepository.save(newLike);
                    // 이벤트 발행
                    resumeEventPublisher.publishResumeEvent(memberId, Long.valueOf(resume.getMemberId()), resumeId, NotificationType.LIKE);
                    return true; // 새로운 좋아요 추가됨
                });

        // 좋아요 수 업데이트 및 상태 저장
        resumeRepository.save(resume);
        existingLikeOpt.ifPresent(likeRepository::save);

        return isLikeAdded;
    }
}
