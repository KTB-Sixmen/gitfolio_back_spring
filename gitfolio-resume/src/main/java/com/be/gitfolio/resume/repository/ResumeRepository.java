package com.be.gitfolio.resume.repository;

import com.be.gitfolio.resume.domain.Resume;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ResumeRepository extends MongoRepository<Resume, String>, ResumeRepositoryCustom {

    Page<Resume> findAllByMemberId(String memberId, Pageable pageable);
}
