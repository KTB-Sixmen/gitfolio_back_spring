package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.resume.service.port.MemberClient;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.event.ResumeEventPublisher;
import com.be.gitfolio.resume.service.port.CommentRepository;
import com.be.gitfolio.resume.service.port.NotificationClient;
import com.be.gitfolio.resume.service.port.ResumeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    private final MemberClient memberClient;
//    private final NotificationClient notificationClient;

//    // 테스트용(webClient)
//    @Transactional
//    public Long createCommentWithHttp(String resumeId, Long senderId, String senderNickname, CommentRequestDTO commentRequestDTO) {
//
//        Resume resume = resumeRepository.findById(resumeId)
//                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));
//
//        Comment comment = Comment.of(resumeId, senderId, commentRequestDTO);
//        Comment savedComment = commentRepository.save(comment);
//
//        log.info("저장 성공!");
//        ResumeEvent resumeEvent = new ResumeEvent(senderId, senderNickname, Long.valueOf(resume.getMemberId()), resumeId, NotificationType.COMMENT);
//        notificationClient.sendNotification(resumeEvent);
//        log.info("webClient 요청 성공");
//        return savedComment.getId();
//    }


    /**
     * 댓글 작성
     */
    @Transactional
    public Comment createComment(String resumeId, Long senderId, String senderNickname, CommentRequestDTO commentRequestDTO) {

        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        Comment comment = Comment.of(resumeId, senderId, commentRequestDTO);
        Comment savedComment = commentRepository.save(comment);

        resumeEventPublisher.publishResumeEvent(senderId, senderNickname, resume.getMemberId(), resumeId, NotificationType.COMMENT);
        return savedComment;
    }

    /**
     *  댓글 수정
     */
    @Transactional
    public void updateComment(Long commentId, Long memberId, CommentRequestDTO commentRequestDTO) {
        Comment comment = commentRepository.findById(commentId)
                .orElseThrow(() -> new BaseException(ErrorCode.COMMENT_NOT_FOUND));

        comment.validateOwnership(memberId);

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

        comment.validateOwnership(memberId);

        commentRepository.deleteById(commentId);
    }

    /**
     * 이력서별 댓글 조회
     */
    public List<CommentResponseDTO> getCommentList(String resumeId) {
        List<Comment> comments = commentRepository.findAllByResumeId(resumeId);
        return comments.stream()
                .map(comment -> {
                    MemberInfoDTO memberInfoDTO = memberClient.getMemberInfo(comment.getMemberId());
                    // Avatar URL 가공
                    String avatarUrl = s3Service.getProcessAvatarUrl(memberInfoDTO.avatarUrl());
                    return new CommentResponseDTO(comment, memberInfoDTO.nickname(), avatarUrl);
                })
                .toList();
    }
}
