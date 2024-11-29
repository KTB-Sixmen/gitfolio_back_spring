package com.be.gitfolio.member.controller.port;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.member.domain.Member;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import static com.be.gitfolio.member.controller.response.MemberResponse.*;
import static com.be.gitfolio.member.domain.MemberAdditionalInfoRequest.*;
import static com.be.gitfolio.member.domain.MemberRequest.*;

public interface MemberService {

    Member createMember(MemberCreate memberCreate);

    Optional<Member> findMemberIdByUsername(String username);

    MemberDetail sendMemberDetailToResume(Long memberId);

    MemberDetail getMemberDetails(Long memberId);

    void updateMemberInfo(Long memberId,
                          MemberUpdate memberUpdate,
                          MemberAdditionalInfoUpdate memberAdditionalInfoUpdate,
                          MultipartFile imageFile) throws IOException;

    List<MemberGithubRepository> getUserRepositoriesWithLanguages(String username);

    Void updateMemeberPlan(Long memberId, PaidPlan paidPlan);

    void deleteMember(Long memberId);

    Void decreaseRemainingCount(Long memberId);
}
