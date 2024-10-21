package com.be.gitfolio.resume.service;

import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.repository.LikeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class LikeService {

    private final LikeRepository likeRepository;


    /**
     * 좋아요 기능
     */
    @Transactional
    public boolean toggleLike(String resumeId, Long memberId) {
        Optional<Like> existingLikeOpt = likeRepository.findByResumeIdAndMemberId(resumeId, memberId);

        if (existingLikeOpt.isPresent()) { // 좋아요 존재하면 상태 변경.
            Like existingLike = existingLikeOpt.get();
            existingLike.updateStatus();
            likeRepository.save(existingLike);
            return false;
        } else { // 좋아요 존재안하면
            Like newLike = Like.builder()
                    .resumeId(resumeId)
                    .memberId(memberId)
                    .status(Boolean.TRUE)
                    .build();
            likeRepository.save(newLike);
            return true;
        }
    }
}
