package com.be.gitfolio.resume.mapper;

import com.be.gitfolio.common.grpc.MemberServiceProto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

@Mapper(componentModel = "spring", uses = {MemberMapper.class})
public interface MemberInfoMapper {

    @Mapping(target = "position", expression = "java(com.be.gitfolio.common.type.PositionType.fromString(memberResponse.getPosition()))")
    @Mapping(target = "workExperiences", source = "workExperiencesList")
    @Mapping(target = "links", source = "linksList")
    @Mapping(target = "educations", source = "educationsList")
    @Mapping(target = "certificates", source = "certificatesList")
    MemberInfoDTO toMemberInfoDTO(MemberServiceProto.MemberResponseById memberResponse);
}
