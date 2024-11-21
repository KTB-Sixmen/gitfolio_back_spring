package com.be.gitfolio.member.domain;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class Link {
    private String linkTitle;
    private String linkUrl;
}
