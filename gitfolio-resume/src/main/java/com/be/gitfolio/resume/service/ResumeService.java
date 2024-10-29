package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.client.MemberGrpcClient;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.mapper.MemberMapper;
import com.be.gitfolio.resume.repository.CommentRepository;
import com.be.gitfolio.resume.repository.LikeRepository;
import com.be.gitfolio.resume.repository.ResumeRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;
import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Service
@Slf4j
@Transactional(readOnly = true)
public class ResumeService {

    private final MemberGrpcClient memberGrpcClient;
    private final ResumeRepository resumeRepository;
    private final LikeRepository likeRepository;
    private final CommentRepository commentRepository;
    private final RedisTemplate<String, String> redisTemplate;
    private final WebClient webClient;

    public ResumeService(MemberGrpcClient memberGrpcClient,
                         ResumeRepository resumeRepository,
                         LikeRepository likeRepository,
                         CommentRepository commentRepository,
                         RedisTemplate<String, String> redisTemplate,
                         @Value("${ai.server.url}") String url,
                         WebClient.Builder webClientBuilder) {
        this.memberGrpcClient = memberGrpcClient;
        this.resumeRepository = resumeRepository;
        this.likeRepository = likeRepository;
        this.commentRepository = commentRepository;
        this.redisTemplate = redisTemplate;
        this.webClient = webClientBuilder.baseUrl(url).build();
    }

    /**
     * 이력서 생성 요청
     */
    @Transactional
    public String createResume(String memberId, CreateResumeRequestDTO createResumeRequestDTO) {

        // Member 가져와서 personalRepo 만들기
        MemberResponseById memberResponse = memberGrpcClient.getMember(memberId);
        String personalRepo = "https://github.com/" + memberResponse.getNickname();

        // proto -> MemberInfoDTO
        MemberInfoDTO memberInfoDTO = convertToMemberInfoDTO(memberResponse);

        AIRequestDTO aiRequestDTO = AIRequestDTO.builder()
                .githubID(memberResponse.getNickname())
                .personalRepo(personalRepo)
                .selectedRepo(createResumeRequestDTO.getSelectedRepo())
                .requirements(createResumeRequestDTO.getRequirements())
                .build();


        AIResponseDTO aiResponseDTO = webClient.post()
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
    public PaginationResponseDTO<ResumeListDTO> getResumeList(ResumeFilterDTO resumeFilterDTO) {
        Pageable pageable = PageRequest.of(resumeFilterDTO.getPage(), resumeFilterDTO.getSize());
        Page<ResumeListDTO> resumePage = resumeRepository.findResumeByFilter(resumeFilterDTO, pageable).map(ResumeListDTO::new);

        return new PaginationResponseDTO<>(resumePage);
    }

    /**
     * 이력서 상세 조회
     */
    @Transactional
    public ResumeDetailDTO getResumeDetail(String resumeId,String clientIp) {
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));
        incrementViewCount(resume, clientIp);
        resumeRepository.save(resume);
        return new ResumeDetailDTO(resume);
    }

    /**
     * 내 이력서 목록 조회
     */
    public List<ResumeListDTO> getMyResumeList(String memberId) {
        List<Resume> resumes = resumeRepository.findAllByMemberId(memberId);

        return resumes.stream()
                .map(ResumeListDTO::new)
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
    public void updateResume(String resumeId, UpdateResumeRequestDTO updateResumeRequestDTO) {
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));
        resume.updateResume(updateResumeRequestDTO);
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
                    } else {
                        resume.decreaseLike();
                    }
                    return false; // 좋아요 취소됨
                })
                .orElseGet(() -> {
                    Like newLike = Like.builder()
                            .resumeId(resumeId)
                            .memberId(memberId)
                            .status(Boolean.TRUE)
                            .build();
                    resume.increaseLike();
                    likeRepository.save(newLike);
                    return true; // 새로운 좋아요 추가됨
                });

        // 좋아요 수 업데이트 및 상태 저장
        resumeRepository.save(resume);
        existingLikeOpt.ifPresent(likeRepository::save);

        return isLikeAdded;
    }

    private MemberInfoDTO convertToMemberInfoDTO(MemberResponseById memberResponse) {
        return MemberInfoDTO.builder()
                .memberId(memberResponse.getMemberId())
                .memberName(memberResponse.getMemberName())
                .avatarUrl(memberResponse.getAvatarUrl())
                .phoneNumber(memberResponse.getPhoneNumber())
                .email(memberResponse.getEmail())
                .position(memberResponse.getPosition())
                .workExperiences(toEntityList(memberResponse.getWorkExperiencesList(), MemberMapper::toEntity))
                .links(toEntityList(memberResponse.getLinksList(), MemberMapper::toEntity))
                .educations(toEntityList(memberResponse.getEducationsList(), MemberMapper::toEntity))
                .certificates(toEntityList(memberResponse.getCertificatesList(), MemberMapper::toEntity))
                .build();
    }

    private <T, R> List<R> toEntityList(List<T> sourceList, Function<T, R> mapper) {
        return sourceList.stream().map(mapper).toList();
    }
}
