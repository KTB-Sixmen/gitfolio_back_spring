package com.be.gitfolio.member.controller.response;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.member.domain.*;

import java.util.List;
import java.util.Map;



public class MemberResponse {

    public record MemberDetail(
            Long memberId,
            String memberAdditionalInfoId,
            String nickname,
            String name,
            String username,
            String githubName,
            String avatarUrl,
            String phoneNumber,
            String email,
            PositionType position,
            PaidPlan paidPlan,
            Integer remainingCount,
            List<WorkExperience> workExperiences,
            List<Education> educations,
            List<Certificate> certificates,
            List<Link> links
    ) {
        public static MemberDetail of(Member member, MemberAdditionalInfo memberAdditionalInfo, String avatarFullUrl) {
            return new MemberDetail(
                    member.getId(),
                    memberAdditionalInfo.getId(),
                    member.getNickname(),
                    member.getName(),
                    member.getUsername(),
                    member.getGithubName(),
                    avatarFullUrl,
                    member.getPhoneNumber(),
                    member.getEmail(),
                    member.getPosition(),
                    member.getPaidPlan(),
                    member.getRemainingCount(),
                    memberAdditionalInfo.getWorkExperiences(),
                    memberAdditionalInfo.getEducations(),
                    memberAdditionalInfo.getCertificates(),
                    memberAdditionalInfo.getLinks()
            );
        }
    }

    public record MemberGithubRepository(
            Long repoId,
            String repoName,
            String repoUrl,
            String topLanguage,
            String updatedAt
    ) {
        public static MemberGithubRepository from(Map<String, Object> repo) {
            return new MemberGithubRepository(
                    Long.valueOf((Integer) repo.get("id")),
                    (String) repo.get("name"),
                    (String) repo.get("html_url"),
                    (String) repo.get("language"),
                    (String) repo.get("updated_at")
            );
        }
    }
}