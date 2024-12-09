package com.be.gitfolio.resume.repository.resume;

import com.be.gitfolio.resume.domain.Resume;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ResumeMongoRepository extends MongoRepository<Resume, String> {

    Page<Resume> findAllByMemberId(String memberId, Pageable pageable);
}
