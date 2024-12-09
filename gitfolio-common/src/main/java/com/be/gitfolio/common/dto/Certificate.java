package com.be.gitfolio.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
@AllArgsConstructor
public class Certificate {
    private String certificateName;
    private String certificateGrade;
    private String certificatedAt;
    private String certificateOrganization;
}
