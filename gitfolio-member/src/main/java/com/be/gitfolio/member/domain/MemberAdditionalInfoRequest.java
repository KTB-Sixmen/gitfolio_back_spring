package com.be.gitfolio.member.domain;


import lombok.Builder;

import java.util.List;

public class MemberAdditionalInfoRequest {

    @Builder
    public record MemberAdditionalInfoUpdate(
            List<WorkExperience> workExperiences,
            List<Education> educations,
            List<Certificate> certificates,
            List<Link> links
    ) {}
}
