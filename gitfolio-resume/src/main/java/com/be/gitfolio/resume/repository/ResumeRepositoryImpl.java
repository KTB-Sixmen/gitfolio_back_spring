package com.be.gitfolio.resume.repository;

import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import java.util.List;

@RequiredArgsConstructor
@Slf4j
public class ResumeRepositoryImpl implements ResumeRepositoryCustom {

    private final MongoTemplate mongoTemplate;

    @Override
    public Page<Resume> findResumeByFilter(ResumeRequestDTO.ResumeFilterDTO resumeFilterDTO, Pageable pageable) {
        Query query = new Query();

        // 동적 필터 적용
        if (resumeFilterDTO.getTag() != null && !resumeFilterDTO.getTag().isEmpty()) {
            query.addCriteria(Criteria.where("tag").regex(resumeFilterDTO.getTag(), "i"));
        }

        if (resumeFilterDTO.getPosition() != null && !resumeFilterDTO.getPosition().isEmpty()) {
            query.addCriteria(Criteria.where("position").regex(resumeFilterDTO.getPosition(), "i"));
        }

        if (resumeFilterDTO.getTechStack() != null && !resumeFilterDTO.getTechStack().isEmpty()) {
            query.addCriteria(Criteria.where("techStack").regex(resumeFilterDTO.getTechStack(), "i"));
        }

        if (resumeFilterDTO.getSchoolType() != null && !resumeFilterDTO.getSchoolType().isEmpty()) {
            query.addCriteria(Criteria.where("educations.schoolType").regex(resumeFilterDTO.getSchoolType(), "i"));
        }

        // 정렬 기준
        if (resumeFilterDTO.getSortOrder() != null) {
            switch (resumeFilterDTO.getSortOrder()) {
                case "recent" -> query.with(Sort.by(Sort.Direction.DESC, "createdAt"));
                case "like" -> query.with(Sort.by(Sort.Direction.DESC, "likeCount"));
                case "view" -> query.with(Sort.by(Sort.Direction.DESC, "viewCount"));
                default -> {
                }
            }
        }

        // 페이징 처리
        query.with(pageable);
        log.info("Generated Query : {}", query);

        // 결과 조회
        List<Resume> resumes = mongoTemplate.find(query, Resume.class);

        // 총 개수 조회
        long total = mongoTemplate.count(query.skip(-1).limit(-1), Resume.class);

        // 페이지 형식으로 반환
        return new PageImpl<>(resumes, pageable, total);
    }
}
