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

import static net.codecadenza.eclipse.shared.Constants.QUOTE;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the class that is responsible for the security configuration of an application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SecurityConfigurationGenerator extends AbstractJavaSourceGenerator {
	private static final String BOOLEAN_TYPE = "bool";

	private final Project project;
	private final BuildArtifactType artifactType;
	private final DomainObject userObject;
	private final String packageName;
	private final boolean securityEnabled;

	/**
	 * Constructor
	 * @param project
	 */
	public SecurityConfigurationGenerator(Project project) {
		this.project = project;
		this.userObject = project.getDomainObjectByTag(DomainTagEnumeration.USER);
		this.securityEnabled = project.getApplicationLogOnDTO() != null;

		if (project.hasJSFOrVaadinClient()) {
			this.artifactType = BuildArtifactType.GUI;
			this.packageName = project.getClientNamespace().toString();
		}
		else {
			this.artifactType = BuildArtifactType.SERVER;
			this.packageName = project.getRootNamespace().toString();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var javaFile = new JavaFile(project, artifactType, "SecurityConfiguration", packageName);
		javaFile.setComment("This class is responsible for the security configuration of this application");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		if (project.isSpringBootApplication()) {
			importPackage("org.springframework.context.annotation");
			importPackage("org.springframework.security.config.annotation.web.builders");
			importPackage("org.springframework.security.config.annotation.web.configuration");
			importPackage("org.springframework.security.config.annotation.web.configurers");
			importPackage("org.springframework.security.web");

			if (securityEnabled) {
				importPackage("org.springframework.security.crypto.factory");
				importPackage("org.springframework.security.crypto.password");

				if (project.hasVaadinClient())
					importPackage(project.getClientNamespace().toString() + SUB_PACKAGE_UTIL);

				if (project.hasJSFOrVaadinClient()) {
					importPackage("org.springframework.security.web.util.matcher");
					importPackage("java.util.stream");
				}
			}

			if (artifactType == BuildArtifactType.SERVER || securityEnabled) {
				importPackage("org.springframework.security.authentication");
				importPackage("org.springframework.security.config.annotation.authentication.configuration");
			}
		}
		else {
			importPackage("jakarta.enterprise.context");
			importPackage("jakarta.security.enterprise.authentication.mechanism.http");
			importPackage("jakarta.security.enterprise.identitystore");
			importPackage("net.codecadenza.runtime.server.security");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (project.isJakartaEEApplication()) {
			b.append(getJakartaEESecurityAnnotations());
			b.append("@ApplicationScoped\n");
		}
		else {
			b.append("@Configuration\n");
			b.append("@EnableWebSecurity\n");
		}

		b.append("public class SecurityConfiguration");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (!project.isSpringBootApplication() || !securityEnabled)
			return;

		final var unprotectedResources = new StringBuilder();

		if (project.hasVaadinClient()) {
			unprotectedResources.append("{\"/VAADIN/**\", \"/favicon.ico\", \"/manifest.webmanifest\", \"/sw.js\"");
			unprotectedResources.append(", \"/offline.html\", \"/images/**\"");
		}
		else if (project.hasJSFClient())
			unprotectedResources.append("{\"/jakarta.faces.resource/**\", \"/css/default.css\"");

		if (project.hasJSFOrVaadinClient()) {
			if (project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_REST) != null)
				unprotectedResources.append(", \"/rest/**\"");

			if (project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_SOAP) != null)
				unprotectedResources.append(", \"/ws/**\"");

			unprotectedResources.append("}");
		}

		if (project.hasJSFClient()) {
			addPrivateConstant(JavaType.STRING, "LOGIN_PAGE", "\"/login.jsf\"").create();
			addPrivateConstant(JavaType.STRING, "LOGIN_PROCESSING_URL", "\"/login.xhtml\"").create();
			addPrivateConstant(JavaType.STRING + "[]", "UNPROTECTED_RESOURCES", unprotectedResources.toString()).create();
		}
		else if (project.hasVaadinClient()) {
			addPrivateConstant(JavaType.STRING, "LOGIN_PAGE", "\"/login\"").create();
			addPrivateConstant(JavaType.STRING, "LOGIN_PROCESSING_URL", "\"/login\"").create();
			addPrivateConstant(JavaType.STRING + "[]", "UNPROTECTED_RESOURCES", unprotectedResources.toString()).create();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		if (!project.isSpringBootApplication())
			return;

		var methodSignature = "SecurityFilterChain filterChain(HttpSecurity http)";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Configure HTTP security\n");
		b.append(" * @param http the object that can be configured\n");
		b.append(" * @return a {@link SecurityFilterChain}\n");
		b.append(" * @throws Exception if the configuration could not be applied\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + " throws Exception\n");
		b.append("{\n");

		if (securityEnabled && project.hasJSFOrVaadinClient()) {
			if (project.hasVaadinClient())
				b.append("http.requestCache(cacheConfig -> new ApplicationRequestCache());\n\n");

			b.append("// Require all requests to be authenticated except for the resources\n");
			b.append("http.authorizeHttpRequests(matcherRegistry -> {\n");

			if (project.hasVaadinClient())
				b.append("matcherRegistry.requestMatchers(ApplicationRequestCache::isFrameworkInternalRequest).permitAll();\n");

			b.append("matcherRegistry.requestMatchers(getRequestMatchersForUnprotectedResources())");
			b.append(".permitAll().anyRequest().authenticated();\n");
			b.append("});\n\n");
			b.append("// Use the respective page to login\n");
			b.append("http.formLogin(loginConfig -> {\n");
			b.append("loginConfig.loginPage(LOGIN_PAGE);\n");
			b.append("loginConfig.loginProcessingUrl(LOGIN_PROCESSING_URL);\n");
			b.append("loginConfig.permitAll();\n");
			b.append("loginConfig.failureUrl(LOGIN_PAGE);\n");
			b.append("});\n\n");
			b.append("// After logging out, the user should be redirected to the login page\n");
			b.append("http.logout(logoutConfig -> logoutConfig.logoutSuccessUrl(LOGIN_PAGE));\n\n");
		}
		else
			b.append("http.authorizeHttpRequests(matcherRegistry -> matcherRegistry.anyRequest().permitAll());\n\n");

		b.append("// CSRF is handled internally\n");
		b.append("http.csrf(CsrfConfigurer::disable);\n\n");
		b.append("return http.build();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (securityEnabled) {
			if (project.isSpringBootApplication() && project.hasJSFOrVaadinClient()) {
				methodSignature = "AntPathRequestMatcher[] getRequestMatchersForUnprotectedResources()";

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * @return an array of {@link AntPathRequestMatcher} for all unprotected resources\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private AntPathRequestMatcher[] getRequestMatchersForUnprotectedResources()\n");
				b.append("{\n");
				b.append("return Stream.of(UNPROTECTED_RESOURCES).map(AntPathRequestMatcher::new)");
				b.append(".toArray(size -> new AntPathRequestMatcher[size]);\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}

			methodSignature = "PasswordEncoder passwordEncoder()";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Producer method for injecting a {@link PasswordEncoder}\n");
			b.append(" * @return a password encoder\n");
			b.append(" */\n");
			b.append("@Bean\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("return PasswordEncoderFactories.createDelegatingPasswordEncoder();\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		if (artifactType == BuildArtifactType.SERVER || securityEnabled) {
			methodSignature = "AuthenticationManager authenticationManager(AuthenticationConfiguration authenticationConfiguration)";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Producer method for injecting an {@link AuthenticationManager}\n");
			b.append(" * @param authenticationConfiguration\n");
			b.append(" * @return an authentication manager\n");
			b.append(" * @throws Exception if the given configuration could not provide an authentication manager\n");
			b.append(" */\n");
			b.append("@Bean\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + " throws Exception\n");
			b.append("{\n");
			b.append("return authenticationConfiguration.getAuthenticationManager();\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}
	}

	/**
	 * @return all security-related Jakarta EE annotations
	 */
	private String getJakartaEESecurityAnnotations() {
		final var b = new StringBuilder();
		final DomainAttribute nameAttribute = userObject.getAllAttributes().stream()
				.filter(attr -> attr.getTag() == AttributeTagEnumeration.USER_NAME).findFirst().orElse(null);
		final DomainAttribute passwordAttribute = userObject.getAllAttributes().stream()
				.filter(attr -> attr.getTag() == AttributeTagEnumeration.USER_PASSWORD).findFirst().orElse(null);
		final DomainAttribute activeAttribute = userObject.getAllAttributes().stream()
				.filter(attr -> attr.getTag() == AttributeTagEnumeration.USER_ACTIVE).findFirst().orElse(null);
		final AbstractDomainAssociation roleAssoc = userObject.getAllAssociations().stream()
				.filter(assoc -> assoc.getTag() == AssociationTagEnumeration.USER_ROLE).findFirst().orElse(null);

		if (nameAttribute == null || passwordAttribute == null || roleAssoc == null || activeAttribute == null)
			throw new IllegalStateException("The domain object '" + userObject.getName() + "' is not tagged properly!");

		final DomainAttribute roleNameAttr = roleAssoc.getTarget().getAllAttributes().stream()
				.filter(attr -> attr.getTag() == AttributeTagEnumeration.ROLE_NAME).findFirst().orElse(null);
		final DBTable roleTable = roleAssoc.getTarget().getDatabaseTable();
		final DBColumn userIdColumn = userObject.getDatabaseTable().getPrimaryKey().getColumn();

		if (roleNameAttr == null)
			throw new IllegalStateException("The domain object '" + roleAssoc.getTarget().getName() + "' is not tagged properly!");

		b.append("@CustomFormAuthenticationMechanismDefinition(loginToContinue = @LoginToContinue(loginPage = \"");

		if (project.hasJSFClient())
			b.append("/login.jsf");
		else if (project.hasVaadinClient())
			b.append("/login");

		b.append("\", errorPage = \"\"))\n");
		b.append("@DatabaseIdentityStoreDefinition(dataSourceLookup = \"${'" + project.getDataSourceJNDIName() + "'}\", ");
		b.append("callerQuery = \"#{'select " + getJDBCName(passwordAttribute));
		b.append(" from " + getJDBCName(userObject.getDatabaseTable()) + " ");
		b.append("where " + getJDBCName(nameAttribute) + " =?'}\", groupsQuery = \"");
		b.append("select b." + getJDBCName(roleNameAttr) + " from " + getJDBCName(userObject.getDatabaseTable()));
		b.append(" a, " + getJDBCName(roleTable) + " b ");

		if (roleAssoc instanceof final ManyToManyAssociation mtm) {
			// By definition, the first column always represents the user ID!
			final DBColumn userRefCol = mtm.getTable().getColumns().get(0);

			// The many-to-many relationship table always has exactly two columns!
			final DBColumn roleRefCol = mtm.getTable().getColumns().get(1);

			b.append(", " + getJDBCName(mtm.getTable()) + " c ");
			b.append("where a." + getJDBCName(userIdColumn) + " = c." + getJDBCName(userRefCol) + " ");
			b.append("and b." + getJDBCName(roleTable.getPrimaryKey().getColumn()) + " = c." + getJDBCName(roleRefCol) + " ");
		}
		else {
			final var mto = (ManyToOneAssociation) roleAssoc;

			b.append("where a." + getJDBCName(mto.getColumn()) + " = b." + getJDBCName(roleTable.getPrimaryKey().getColumn()) + " ");
		}

		b.append("and a." + getJDBCName(nameAttribute) + " =? ");
		b.append("and a." + getJDBCName(activeAttribute) + " ");

		// The database type of the field that determines if a user account is active or not can be either an integer or a boolean!
		if (activeAttribute.getColumn().getColumnType().getName().toLowerCase().contains(BOOLEAN_TYPE))
			b.append("is true");
		else
			b.append("= 1");

		b.append("\", hashAlgorithm = SHA256PasswordHash.class)\n");

		return b.toString();
	}

	/**
	 * Determine the name of a given column that can be used in a JDBC query statement
	 * @param column
	 * @return the JDBC name
	 */
	private static final String getJDBCName(DBColumn column) {
		return column.getMappingName();
	}

	/**
	 * Determine the JDBC name of the column that is mapped to the given domain attribute
	 * @param attribute
	 * @return the JDBC name
	 */
	private static final String getJDBCName(DomainAttribute attribute) {
		return getJDBCName(attribute.getColumn());
	}

	/**
	 * Determine the name of a given table that can be used in a JDBC query statement
	 * @param table
	 * @return the JDBC name
	 */
	private static final String getJDBCName(DBTable table) {
		return table.getFullDatabaseName().replace(QUOTE, "");
	}

}
