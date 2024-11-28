package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.event.ResumeEventPublisher;
import com.be.gitfolio.resume.repository.CommentRepository;
import com.be.gitfolio.resume.repository.ResumeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.List;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class CommentService {

    private final ResumeRepository resumeRepository;
    private final CommentRepository commentRepository;
    private final ResumeEventPublisher resumeEventPublisher;
    private final S3Service s3Service;
    private final WebClient memberWebClient;

    /**
     * 댓글 작성
     */
    @Transactional
    public Long createComment(String resumeId, Long memberId, CommentRequestDTO commentRequestDTO) {
        MemberInfoDTO memberInfoDTO;
        try {
            memberInfoDTO = memberWebClient.get()
                    .uri("/api/members/{memberId}", memberId)
                    .retrieve()
                    .bodyToMono(MemberInfoDTO.class)
                    .block();
        } catch (WebClientResponseException exception) {
            throw new BaseException(ErrorCode.MEMBER_SERVER_ERROR);
        }

        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        Comment comment = Comment.builder()
                .resumeId(resumeId)
                .memberId(memberId)
                .content(commentRequestDTO.content())
                .build();
        Comment savedComment = commentRepository.save(comment);

        resumeEventPublisher.publishResumeEvent(memberId, memberInfoDTO.nickname(), Long.valueOf(resume.getMemberId()), resumeId, NotificationType.COMMENT);
        return savedComment.getId();
    }

    /**
     *  댓글 수정
     */
    @Transactional
    public void updateComment(Long commentId, Long memberId, CommentRequestDTO commentRequestDTO) {
        Comment comment = commentRepository.findById(commentId)
                .orElseThrow(() -> new BaseException(ErrorCode.COMMENT_NOT_FOUND));

        if (!comment.getMemberId().equals(memberId)) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_UPDATE_COMMENT);
        }

        comment.updateContent(commentRequestDTO.content());
        commentRepository.save(comment);
    }

    /**
     * 댓글 삭제
     */
    @Transactional
    public void deleteComment(Long commentId, Long memberId) {
        Comment comment = commentRepository.findById(commentId)
                .orElseThrow(() -> new BaseException(ErrorCode.COMMENT_NOT_FOUND));

        if (!comment.getMemberId().equals(memberId)) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_UPDATE_COMMENT);
        }

        commentRepository.deleteById(commentId);
    }

    /**
     * 이력서별 댓글 조회
     */
    public List<CommentResponseDTO> getCommentList(String resumeId) {
        List<Comment> comments = commentRepository.findAllByResumeId(resumeId);
        return comments.stream()
                .map(comment -> {
                    MemberInfoDTO memberInfoDTO = memberWebClient.get()
                            .uri("/api/members/{memberId}", comment.getMemberId())
                            .retrieve()
                            .bodyToMono(MemberInfoDTO.class)
                            .block();
                    // Avatar URL 가공
                    String avatarUrl = memberInfoDTO.avatarUrl();
                    if (!avatarUrl.contains("avatars.githubusercontent.com")) {
                        avatarUrl = s3Service.getFullFileUrl(avatarUrl);
                    }
                    return new CommentResponseDTO(comment, memberInfoDTO.nickname(), avatarUrl);
                })
                .toList();
    }
}
