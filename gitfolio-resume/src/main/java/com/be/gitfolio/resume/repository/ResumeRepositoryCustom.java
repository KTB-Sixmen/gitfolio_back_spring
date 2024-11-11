package com.be.gitfolio.resume.repository;

import com.be.gitfolio.resume.domain.Resume;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

public interface ResumeRepositoryCustom {
    Page<Resume> findResumeByFilter(ResumeFilterDTO resumeFilterDTO, List<String> likedResumeId, Pageable pageable);
}
