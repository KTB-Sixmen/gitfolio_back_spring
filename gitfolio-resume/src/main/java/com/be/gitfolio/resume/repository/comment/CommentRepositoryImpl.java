package com.be.gitfolio.resume.repository.comment;

import com.be.gitfolio.resume.domain.Comment;
import com.be.gitfolio.resume.service.port.CommentRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class CommentRepositoryImpl implements CommentRepository {

    private final CommentJpaRepository commentJpaRepository;

    @Override
    public Comment save(Comment comment) {
        return commentJpaRepository.save(comment);
    }

    @Override
    public Optional<Comment> findById(Long commentId) {
        return commentJpaRepository.findById(commentId);
    }

    @Override
    public void deleteById(Long commentId) {
        commentJpaRepository.deleteById(commentId);
    }

    @Override
    public List<Comment> findAllByResumeId(String resumeId) {
        return commentJpaRepository.findAllByResumeId(resumeId);
    }
}
