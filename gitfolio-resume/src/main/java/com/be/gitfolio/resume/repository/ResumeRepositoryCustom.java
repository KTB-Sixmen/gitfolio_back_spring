package com.be.gitfolio.resume.repository;

import com.be.gitfolio.resume.domain.Resume;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

public interface ResumeRepositoryCustom {
    Page<Resume> findResumeByFilter(ResumeFilterDTO resumeFilterDTO, Pageable pageable);
}
