package com.be.gitfolio.auth.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

public class MemberDTO {

    public record MemberSaveRequestDTO(
            String role,
            String nickname,
            String username,
            String avatarUrl,
            String githubName
    ) {
        public static MemberSaveRequestDTO from(GithubResponse githubResponse) {
            return new MemberSaveRequestDTO(
                    "ROLE_USER",
                    githubResponse.getName(),
                    githubResponse.getGithubId(),
                    githubResponse.getAvatarUrl(),
                    githubResponse.getGithubName()
            );
        }
    }

    public record OAuth2UserDTO(
            Long memberId,
            String role,
            String nickname,
            String username
    ) {}

}
