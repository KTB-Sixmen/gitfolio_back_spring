package com.be.gitfolio.resume.service.port;

import com.be.gitfolio.resume.domain.Comment;

import java.util.List;
import java.util.Optional;

public interface CommentRepository {
    Comment save(Comment comment);

    Optional<Comment> findById(Long commentId);

    void deleteById(Long commentId);

    List<Comment> findAllByResumeId(String resumeId);
}
