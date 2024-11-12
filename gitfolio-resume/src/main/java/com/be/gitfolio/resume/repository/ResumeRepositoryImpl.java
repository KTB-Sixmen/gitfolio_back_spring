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
    public Page<Resume> findResumeByFilter(ResumeRequestDTO.ResumeFilterDTO resumeFilterDTO, List<String> likedResumeIds, Pageable pageable) {
        Query query = new Query();

        // 동적 필터 적용
        if (resumeFilterDTO.tag() != null && !resumeFilterDTO.tag().isEmpty()) {
            query.addCriteria(Criteria.where("tag").is(resumeFilterDTO.tag()));
        }

        if (resumeFilterDTO.position() != null && !resumeFilterDTO.position().isEmpty()) {
            query.addCriteria(Criteria.where("position").is(resumeFilterDTO.position()));
        }

        if (resumeFilterDTO.techStack() != null && !resumeFilterDTO.techStack().isEmpty()) {
            query.addCriteria(Criteria.where("techStack").elemMatch(Criteria.where("$eq").is(resumeFilterDTO.techStack())));
        }

        if (resumeFilterDTO.schoolType() != null && !resumeFilterDTO.schoolType().isEmpty()) {
            query.addCriteria(Criteria.where("educations").elemMatch(Criteria.where("schoolType").is(resumeFilterDTO.schoolType())));
        }

        // 정렬 기준
        if (resumeFilterDTO.sortOrder() != null) {
            switch (resumeFilterDTO.sortOrder()) {
                case "recent" -> query.with(Sort.by(Sort.Direction.DESC, "updatedAt"));
                case "like" -> query.with(Sort.by(Sort.Direction.DESC, "likeCount"));
                case "view" -> query.with(Sort.by(Sort.Direction.DESC, "viewCount"));
                default -> {
                }
            }
        }

        if (likedResumeIds != null) {
            query.addCriteria(Criteria.where("_id").in(likedResumeIds));
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