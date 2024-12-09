package com.be.gitfolio.resume.repository.comment;

import com.be.gitfolio.resume.domain.Comment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CommentJpaRepository extends JpaRepository<Comment, Long> {
    List<Comment> findAllByResumeId(String resumeId);

    void deleteCommentsByResumeId(String resumeId);
}
