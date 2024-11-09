package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.client.MemberGrpcClient;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.jwt.JWTUtil;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.event.ResumeEventPublisher;
import com.be.gitfolio.resume.mapper.MemberInfoMapper;
import com.be.gitfolio.resume.repository.CommentRepository;
import com.be.gitfolio.resume.repository.LikeRepository;
import com.be.gitfolio.resume.repository.ResumeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.reactive.function.client.WebClient;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

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
    private final MongoTemplate mongoTemplate;

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

        // 1. MySQL에서 사용자가 좋아요 누른 이력서 ID 목록 조회
        List<String> likedResumeIds = null;
        if (Boolean.TRUE.equals(resumeFilterDTO.liked())) {
            likedResumeIds = likeRepository.findLikedResumeIdsByMemberId(memberId);
            // 좋아요 목록 비어있으면 빈 페이지 반환
            if (likedResumeIds.isEmpty()) {
                return new PaginationResponseDTO<>(List.of(), 0, pageable);
            }
        }

        // 2. MongoDB에서 이력서 목록 조회
        Page<Resume> resumePage = resumeRepository.findResumeByFilter(resumeFilterDTO, likedResumeIds, pageable);

        // 3. MySQL에서 사용자가 좋아요 누른 상태를 한 번에 조회하고 Map으로 변환
        List<Like> likes = likeRepository
                .findLikesByMemberIdAndResumeIds(memberId, resumePage.stream().map(Resume::getId).toList());

        // List<Like>를 Map<String, Boolean>으로 변환
        Map<String, Boolean> likedStatusMap = likes.stream()
                .collect(Collectors.toMap(Like::getResumeId, Like::getStatus));


        // 4. 이력서 목록을 DTO로 변환
        List<ResumeListDTO> resumeListDTOs = resumePage.stream()
                .map(resume -> {
                    boolean isLiked = likedStatusMap.getOrDefault(resume.getId(), false);

                    // Avatar URL 가공
                    String avatarUrl = resume.getAvatarUrl();
                    if (avatarUrl != null && !avatarUrl.contains("avatars.githubusercontent.com")) {
                        avatarUrl = s3Service.getFullFileUrl(avatarUrl);
                    }

                    return new ResumeListDTO(resume, isLiked, avatarUrl);
                })
                .toList();

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
        }
    }

    /**
     * 좋아요 기능
     */
    @Transactional
    public boolean toggleLike(String resumeId, Long memberId) {

        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        Optional<Like> existingLikeOpt = likeRepository.findByResumeIdAndMemberId(resumeId, memberId);

        return existingLikeOpt.map(existingLike -> {
            existingLike.updateStatus();
            if (Boolean.TRUE.equals(existingLike.getStatus())) {
                increaseLikeCount(resumeId);
                resumeEventPublisher.publishResumeEvent(memberId, Long.valueOf(resume.getMemberId()), resumeId, NotificationType.LIKE);
            } else {
                decreaseLikeCount(resumeId);
            }
            return false;
        }).orElseGet(() -> {
            Like newLike = Like.of(resumeId, memberId);
            increaseLikeCount(resumeId);
            likeRepository.save(newLike);
            resumeEventPublisher.publishResumeEvent(memberId, Long.valueOf(resume.getMemberId()), resumeId, NotificationType.LIKE);
            return true;
        });
    }

    /**
     * 좋아요 수 증가 (MongoTemplate 활용)
     */
    private void increaseLikeCount(String resumeId) {
        Query query = new Query(Criteria.where("_id").is(resumeId));
        Update update = new Update().inc("likeCount", 1);
        mongoTemplate.findAndModify(query, update, Resume.class);
    }

    /**
     * 좋아요 수 감소 (MongoTemplate 활용)
     */
    private void decreaseLikeCount(String resumeId) {
        Query query = new Query(Criteria.where("_id").is(resumeId));
        Update update = new Update().inc("likeCount", -1);
        mongoTemplate.findAndModify(query, update, Resume.class);
    }
}
