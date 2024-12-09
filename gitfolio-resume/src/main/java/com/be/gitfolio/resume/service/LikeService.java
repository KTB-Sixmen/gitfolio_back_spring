package com.be.gitfolio.resume.service;

import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.service.port.MemberClient;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.resume.domain.Like;
import com.be.gitfolio.resume.domain.Resume;
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

import java.util.Optional;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class LikeService {

    private final ResumeRepository resumeRepository;
    private final LikeRepository likeRepository;
    private final MongoTemplate mongoTemplate;
    private final ResumeEventPublisher resumeEventPublisher;
    private final MemberClient memberClient;

    /**
     * 좋아요 기능
     */
    @Transactional
    public boolean toggleLike(String resumeId, Long memberId) {

        MemberInfoDTO memberInfoDTO = memberClient.getMemberInfo(memberId);

        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));

        Optional<Like> existingLikeOpt = likeRepository.findByResumeIdAndMemberId(resumeId, memberId);

        if (existingLikeOpt.isPresent()) {
            handleUnlike(resumeId, memberId);
            return false; // 좋아요 취소
        } else {
            handleLike(resumeId, memberId, memberInfoDTO, resume);
            return true; // 좋아요 추가
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

    private void handleUnlike(String resumeId, Long memberId) {
        likeRepository.deleteByResumeIdAndMemberId(resumeId, memberId); // 좋아요 삭제
        decreaseLikeCount(resumeId); // 좋아요 수 감소
    }

    private void handleLike(String resumeId, Long memberId, MemberInfoDTO memberInfoDTO, Resume resume) {
        Like newLike = Like.of(resumeId, memberId); // 새로운 좋아요 객체 생성
        likeRepository.save(newLike); // 좋아요 저장
        increaseLikeCount(resumeId); // 좋아요 수 증가
        resumeEventPublisher.publishResumeEvent( // 좋아요 이벤트 발행
                memberId,
                memberInfoDTO.nickname(),
                resume.getMemberId(),
                resumeId,
                NotificationType.LIKE
        );
    }
}
