package com.be.gitfolio.resume.mapper;

import com.be.gitfolio.common.grpc.MemberServiceProto;
import com.be.gitfolio.resume.domain.Resume.*;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface MemberMapper {

    Certificate toEntity(MemberServiceProto.Certificate certificateProto);

    Education toEntity(MemberServiceProto.Education educationProto);

    Link toEntity(MemberServiceProto.Link linkProto);

    WorkExperience toEntity(MemberServiceProto.WorkExperience workExperienceProto);

}