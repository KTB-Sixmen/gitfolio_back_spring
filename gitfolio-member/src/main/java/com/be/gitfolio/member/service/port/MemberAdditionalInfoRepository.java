package com.be.gitfolio.member.service.port;

import com.be.gitfolio.member.domain.MemberAdditionalInfo;

import java.util.Optional;

public interface MemberAdditionalInfoRepository {
    MemberAdditionalInfo save(MemberAdditionalInfo memberAdditionalInfo);

    Optional<MemberAdditionalInfo> findByMemberId(String memberId);

    void deleteByMemberId(String memberId);
}
