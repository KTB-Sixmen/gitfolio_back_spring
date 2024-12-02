package com.be.gitfolio.member.domain.request;


import com.be.gitfolio.member.domain.Certificate;
import com.be.gitfolio.member.domain.Education;
import com.be.gitfolio.member.domain.Link;
import com.be.gitfolio.member.domain.WorkExperience;
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
