package com.be.gitfolio.member.domain;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class Certificate {
    private String certificateName;
    private String certificateGrade;
    private String certificatedAt;
    private String certificateOrganization;
}
