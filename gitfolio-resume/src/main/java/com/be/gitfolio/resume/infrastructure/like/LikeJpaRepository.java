package com.be.gitfolio.resume.infrastructure.like;

import com.be.gitfolio.resume.domain.Like;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface LikeJpaRepository extends JpaRepository<Like, Long> {
    Optional<Like> findByResumeIdAndMemberId(String resumeId, Long memberId);

    void deleteLikesByResumeId(String resumeId);

    void deleteByResumeIdAndMemberId(String resumeId, Long memberId);

    @Query("SELECT l.resumeId FROM Like l WHERE l.memberId = :memberId")
    List<String> findLikedResumeIdsByMemberId(Long memberId);

    @Query("SELECT l FROM Like l WHERE l.memberId = :memberId AND l.resumeId IN :resumeIds")
    List<Like> findLikesByMemberIdAndResumeIds(Long memberId, List<String> resumeIds);

    boolean existsByResumeIdAndMemberId(String resumeId, Long memberId);

    @Query("SELECT l.resumeId FROM Like l WHERE l.memberId = :memberId AND l.resumeId IN :resumeIds")
    List<String> findLikedResumeIdsByMemberIdAndResumeIds(Long memberId, List<String> resumeIds);

    void deleteAllByMemberId(Long memberId);
}
