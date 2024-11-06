package com.be.gitfolio.resume.repository;

import com.be.gitfolio.resume.domain.Like;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.Set;

@Repository
public interface LikeRepository extends JpaRepository<Like, Long> {
    Optional<Like> findByResumeIdAndMemberId(String resumeId, Long memberId);

    void deleteLikesByResumeId(String resumeId);
}
