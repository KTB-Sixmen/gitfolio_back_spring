package com.be.gitfolio.resume.service.port;

import com.be.gitfolio.resume.domain.Resume;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.Cursor;

import java.util.List;
import java.util.Optional;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

public interface ResumeRepository {
    Page<Resume> findResumeByFilter(ResumeFilterDTO resumeFilterDTO, List<String> likedResumeId, Pageable pageable);

    Resume save(Resume resume);

    Optional<Resume> findById(String resumeId);

    Page<Resume> findAllByMemberId(Long memberId, Pageable pageable);

    void deleteById(String resumeId);

    void deleteAllByMemberId(Long memberId);
}
