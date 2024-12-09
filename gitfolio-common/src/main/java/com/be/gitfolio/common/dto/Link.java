package com.be.gitfolio.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
@AllArgsConstructor
public class Link {
    private String linkTitle;
    private String linkUrl;
}
