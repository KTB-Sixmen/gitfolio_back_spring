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
    @Query("SELECT l.resumeId FROM Like l WHERE l.memberId = :memberId")
    Set<String> findLikedResumeIdsByMemberId(Long memberId);
    boolean existsByMemberIdAndResumeId(Long memberId, String resumeId);
    void deleteLikesByResumeId(String resumeId);
}
