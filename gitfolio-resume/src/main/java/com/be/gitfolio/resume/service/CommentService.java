package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.dto.ResumeResponseDTO;
import com.be.gitfolio.resume.repository.CommentRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class CommentService {

    private final CommentRepository commentRepository;


    /**
     * 댓글 작성
     */
    @Transactional
    public Long createComment(String resumeId, Long memberId, CommentRequestDTO commentRequestDTO) {
        Comment comment = Comment.builder()
                .resumeId(resumeId)
                .memberId(memberId)
                .content(commentRequestDTO.getContent())
                .build();
        return commentRepository.save(comment).getId();
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

        comment.updateContent(commentRequestDTO.getContent());
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
                .map(CommentResponseDTO::new)
                .collect(Collectors.toList());
    }
}
