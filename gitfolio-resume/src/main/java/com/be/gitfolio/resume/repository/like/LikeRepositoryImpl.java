package com.be.gitfolio.resume.repository.like;

import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.service.port.LikeRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class LikeRepositoryImpl implements LikeRepository {

    private final LikeJpaRepository likeJpaRepository;

    @Override
    public List<String> findLikedResumeIdsByMemberId(Long memberId) {
        return likeJpaRepository.findLikedResumeIdsByMemberId(memberId);
    }

    @Override
    public List<String> findLikedResumeIdsByMemberIdAndResumeIds(Long memberId, List<String> resumeIds) {
        return likeJpaRepository.findLikedResumeIdsByMemberIdAndResumeIds(memberId, resumeIds);
    }

    @Override
    public boolean existsByResumeIdAndMemberId(String resumeId, Long memberId) {
        return likeJpaRepository.existsByResumeIdAndMemberId(resumeId, memberId);
    }

    @Override
    public List<Like> findLikesByMemberIdAndResumeIds(Long memberId, List<String> resumeIds) {
        return likeJpaRepository.findLikesByMemberIdAndResumeIds(memberId, resumeIds);
    }

    @Override
    public void deleteLikesByResumeId(String resumeId) {
        likeJpaRepository.deleteLikesByResumeId(resumeId);
    }

    @Override
    public Optional<Like> findByResumeIdAndMemberId(String resumeId, Long memberId) {
        return likeJpaRepository.findByResumeIdAndMemberId(resumeId, memberId);
    }

    @Override
    public void deleteByResumeIdAndMemberId(String resumeId, Long memberId) {
        likeJpaRepository.deleteByResumeIdAndMemberId(resumeId, memberId);
    }

    @Override
    public Like save(Like like) {
        return likeJpaRepository.save(like);
    }
}
