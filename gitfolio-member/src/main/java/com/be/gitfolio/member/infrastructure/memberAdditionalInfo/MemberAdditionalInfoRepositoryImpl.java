package com.be.gitfolio.member.infrastructure.memberAdditionalInfo;

import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import com.be.gitfolio.member.service.port.MemberAdditionalInfoRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class MemberAdditionalInfoRepositoryImpl implements MemberAdditionalInfoRepository {

    private final MemberAdditionalInfoMongoRepository memberAdditionalInfoMongoRepository;

    @Override
    public MemberAdditionalInfo save(MemberAdditionalInfo memberAdditionalInfo) {
        return memberAdditionalInfoMongoRepository.save(MemberAdditionalInfoEntity.fromModel(memberAdditionalInfo)).toModel();
    }

    @Override
    public Optional<MemberAdditionalInfo> findByMemberId(String memberId) {
        return memberAdditionalInfoMongoRepository.findByMemberId(memberId).map(MemberAdditionalInfoEntity::toModel);
    }

    @Override
    public void deleteByMemberId(String memberId) {
        memberAdditionalInfoMongoRepository.deleteByMemberId(memberId);
    }
}
