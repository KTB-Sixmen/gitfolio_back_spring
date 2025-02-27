package com.be.gitfolio.member.infrastructure.memberAdditionalInfo;

import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.Optional;

public interface MemberAdditionalInfoMongoRepository extends MongoRepository<MemberAdditionalInfoEntity, String> {



    Optional<MemberAdditionalInfoEntity> findByMemberId(Long memberId);

    void deleteByMemberId(Long memberId);
}
