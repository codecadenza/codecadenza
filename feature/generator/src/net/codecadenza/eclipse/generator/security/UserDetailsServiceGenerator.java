/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.generator.security;

import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for the implementation of a standard service interface provided by Spring
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UserDetailsServiceGenerator extends AbstractJavaSourceGenerator {
	private static final String USER_DETAILS_SERVICE = "UserDetailsServiceImpl";

	private final Project project;
	private final BoundaryBean logOnBoundary;
	private final DTOBean logOnDTO;
	private final Repository userRepository;
	private final DomainObject user;
	private RepositoryMethod userMethod;
	private AbstractDomainAssociation roleAssoc;
	private String passwordGetter;
	private String activeGetter;
	private String userService;

	/**
	 * Constructor
	 * @param project
	 */
	public UserDetailsServiceGenerator(Project project) {
		this.project = project;
		this.logOnBoundary = project.getLogOnBoundary();
		this.logOnDTO = project.getApplicationLogOnDTO();
		this.userRepository = logOnBoundary.getRepository();
		this.user = logOnDTO.getDomainObject();
		this.userService = userRepository.getName();

		user.getAllAttributes().forEach(attr -> {
			if (attr.getTag() == AttributeTagEnumeration.USER_PASSWORD)
				this.passwordGetter = attr.getGetterName();

			if (attr.getTag() == AttributeTagEnumeration.USER_ACTIVE)
				this.activeGetter = attr.getGetterName();
		});

		for (final AbstractDomainAssociation assoc : user.getAllAssociations()) {
			if (assoc.getTag() != AssociationTagEnumeration.USER_ROLE)
				continue;

			this.roleAssoc = assoc;
			break;
		}

		for (final RepositoryMethod repositoryMethod : userRepository.getRepositoryMethods()) {
			if (repositoryMethod.getMethodType() != RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY)
				continue;

			for (final MethodParameter p : repositoryMethod.getMethodParameters()) {
				final var param = (RepositoryMethodParameter) p;

				if (param.getAttribute() != null && param.getAttribute().equals(user.getDisplayAttribute())) {
					this.userMethod = repositoryMethod;
					break;
				}
			}

			if (userMethod != null)
				break;
		}

		if (!project.isBoundaryMode())
			this.userService = project.getLogOnBoundary().getInterfaceName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getRootNamespace().toString() + PACK_SERVICE + SUB_PACKAGE_BEAN;

		final var javaFile = new JavaFile(project, BuildArtifactType.SERVICE, USER_DETAILS_SERVICE, packageName);
		javaFile.setComment("Implementation of {@link UserDetailsService}");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("org.springframework.security.core");
		importPackage("org.springframework.security.core.authority");
		importPackage("org.springframework.security.core.userdetails");
		importClass("org.springframework.security.core.userdetails.User");
		importClass("org.springframework.security.core.userdetails.User.UserBuilder");
		importPackage("org.springframework.security.crypto.bcrypt");
		importPackage("org.springframework.stereotype");
		importClass("org.springframework.transaction.annotation.Transactional");
		importPackage("java.util");

		if (!project.isBoundaryMode())
			importPackage(project.getLogOnBoundary().getNamespace().toString());
		else
			importPackage(userRepository.getNamespace().toString());

		if (roleAssoc instanceof ManyToManyAssociation)
			importPackage(roleAssoc.getTarget().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Service\n");
		b.append("public class " + USER_DETAILS_SERVICE + " implements UserDetailsService");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateField(userService, "userManager").inject().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.springframework.security.core.userdetails.UserDetailsService#loadUserByUsername(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Transactional\n");
		b.append("public UserDetails loadUserByUsername(String userName)\n");
		b.append("{\n");
		b.append("final var grantedAuthorities = new ArrayList<GrantedAuthority>();\n");
		b.append(user.getNamespace().toString() + "." + user.getName() + " user = null;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("user = userManager." + userMethod.getName() + "(userName);\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while loading data for user '{}'!", "e", "userName");

		b.append("\n");
		b.append("throw new UsernameNotFoundException(\"Error while loading user data!\");\n");
		b.append("}\n\n");
		b.append("if(user == null)\n");
		b.append("throw new UsernameNotFoundException(\"No such user!\");\n\n");

		DomainAttribute roleAttr = roleAssoc.getTarget().getDisplayAttribute();

		if (roleAttr == null)
			roleAttr = roleAssoc.getTarget().getPKAttribute();

		if (roleAssoc instanceof ManyToManyAssociation) {
			b.append("for(final " + roleAssoc.getTarget().getName() + " role : user." + roleAssoc.getGetterName() + ")\n");
			b.append("grantedAuthorities.add(new SimpleGrantedAuthority(role." + roleAttr.getGetterName() + "));\n\n");
		}
		else {
			b.append("grantedAuthorities.add(new SimpleGrantedAuthority(user.");
			b.append(roleAssoc.getGetterName() + "." + roleAttr.getGetterName() + "));\n\n");
		}

		b.append("try\n");
		b.append("{\n");
		b.append("final UserBuilder builder = User.withUsername(userName);\n");
		b.append("builder.password(\"{bcrypt}\" + new BCryptPasswordEncoder().encode(user." + passwordGetter + "));\n");
		b.append("builder.disabled(!user." + activeGetter + ");\n");
		b.append("builder.authorities(grantedAuthorities);\n\n");
		b.append("return builder.build();\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Failed to initialize user details for user '{}'!", "e", "userName");

		b.append("\n");
		b.append("return null;\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod("UserDetails loadUserByUsername(String userName)", b.toString());
	}

}
