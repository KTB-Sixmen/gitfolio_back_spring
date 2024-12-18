package com.be.gitfolio.resume.service.port;

import com.be.gitfolio.resume.domain.Like;

import java.util.List;
import java.util.Optional;

public interface LikeRepository {
    List<String> findLikedResumeIdsByMemberId(Long memberId);

    List<String> findLikedResumeIdsByMemberIdAndResumeIds(Long memberId, List<String> resumeIds);

    boolean existsByResumeIdAndMemberId(String resumeId, Long memberId);

    List<Like> findLikesByMemberIdAndResumeIds(Long memberId, List<String> resumeIds);

    void deleteLikesByResumeId(String resumeId);

    Optional<Like> findByResumeIdAndMemberId(String resumeId, Long memberId);

    void deleteByResumeIdAndMemberId(String resumeId, Long memberId);

    Like save(Like like);

    void deleteAllByMemberId(Long memberId);
}
