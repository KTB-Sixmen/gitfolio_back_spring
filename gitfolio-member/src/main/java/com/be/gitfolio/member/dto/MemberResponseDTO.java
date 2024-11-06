package com.be.gitfolio.member.dto;

import com.be.gitfolio.common.type.PositionType;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;


public class MemberResponseDTO {

    public record MemberDetailDTO(
            Long memberId,
            String memberAdditionalInfoId,
            String nickname,
            String name,
            String username,
            String avatarUrl,
            String phoneNumber,
            String email,
            PositionType position,
            List<MemberAdditionalInfo.WorkExperience> workExperiences,
            List<MemberAdditionalInfo.Education> educations,
            List<MemberAdditionalInfo.Certificate> certificates,
            List<MemberAdditionalInfo.Link> links
    ) {
        public static MemberDetailDTO of(Member member, MemberAdditionalInfo memberAdditionalInfo) {
            return new MemberDetailDTO(
                    member.getId(),
                    memberAdditionalInfo.getId(),
                    member.getNickname(),
                    member.getName(),
                    member.getUsername(),
                    member.getAvatarUrl(),
                    member.getPhoneNumber(),
                    member.getEmail(),
                    member.getPosition(),
                    memberAdditionalInfo.getWorkExperiences(),
                    memberAdditionalInfo.getEducations(),
                    memberAdditionalInfo.getCertificates(),
                    memberAdditionalInfo.getLinks()
            );
        }
    }

    public record MemberGithubRepositoryDTO(
            Long repoId,
            String repoName,
            String repoUrl,
            String topLanguage,
            String updatedAt
    ) {
        public static MemberGithubRepositoryDTO from(Map<String, Object> repo) {
            return new MemberGithubRepositoryDTO(
                    Long.valueOf((Integer) repo.get("id")),
                    (String) repo.get("name"),
                    (String) repo.get("html_url"),
                    (String) repo.get("language"),
                    (String) repo.get("updated_at")
            );
        }
    }
}