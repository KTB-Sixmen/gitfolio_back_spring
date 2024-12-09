package com.be.gitfolio.member.domain.request;


import com.be.gitfolio.common.dto.Certificate;
import com.be.gitfolio.common.dto.Education;
import com.be.gitfolio.common.dto.Link;
import com.be.gitfolio.common.dto.WorkExperience;
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
