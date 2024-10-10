package com.be.gitfolio.member.repository;

import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface MemberAdditionalInfoRepository extends MongoRepository<MemberAdditionalInfo, String> {
    Optional<MemberAdditionalInfo> findByMemberId(String memberId);
}
