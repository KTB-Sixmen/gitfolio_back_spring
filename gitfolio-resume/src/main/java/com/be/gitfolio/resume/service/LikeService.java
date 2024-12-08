package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.event.ResumeEventPublisher;
import com.be.gitfolio.resume.service.port.LikeRepository;
import com.be.gitfolio.resume.service.port.ResumeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class LikeService {

    private final ResumeRepository resumeRepository;
    private final LikeRepository likeRepository;
    private final MongoTemplate mongoTemplate;
    private final ResumeEventPublisher resumeEventPublisher;
    private final WebClient memberWebClient;

    /**
     * 좋아요 기능
     */
    @Transactional
    public boolean toggleLike(String resumeId, Long memberId) {

        ResumeRequestDTO.MemberInfoDTO memberInfoDTO;
        try {
            memberInfoDTO = memberWebClient.get()
                    .uri("/api/members/{memberId}", memberId)
                    .retrieve()
                    .bodyToMono(ResumeRequestDTO.MemberInfoDTO.class)
                    .block();
        } catch (WebClientResponseException exception) {
            throw new BaseException(ErrorCode.MEMBER_SERVER_ERROR);
        }

        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        Optional<Like> existingLikeOpt = likeRepository.findByResumeIdAndMemberId(resumeId, memberId);

        if (existingLikeOpt.isPresent()) {
            // 좋아요 취소 (삭제)
            likeRepository.deleteByResumeIdAndMemberId(resumeId, memberId);
            decreaseLikeCount(resumeId);
            return false;
        } else {
            // 좋아요 추가
            Like newLike = Like.of(resumeId, memberId);
            likeRepository.save(newLike);
            increaseLikeCount(resumeId);
            resumeEventPublisher.publishResumeEvent(
                    memberId,
                    memberInfoDTO.nickname(),
                    Long.valueOf(resume.getMemberId()),
                    resumeId,
                    NotificationType.LIKE
            );
            return true;
        }
    }

    /**
     * 좋아요 수 증가 (findAndModify)
     */
    private void increaseLikeCount(String resumeId) {
        Query query = new Query(Criteria.where("_id").is(resumeId));
        Update update = new Update().inc("likeCount", 1);
        mongoTemplate.findAndModify(query, update, Resume.class);
    }

    /**
     * 좋아요 수 감소 (MongoTemplate 활용)
     */
    private void decreaseLikeCount(String resumeId) {
        Query query = new Query(Criteria.where("_id").is(resumeId));
        Update update = new Update().inc("likeCount", -1);
        mongoTemplate.findAndModify(query, update, Resume.class);
    }
}
