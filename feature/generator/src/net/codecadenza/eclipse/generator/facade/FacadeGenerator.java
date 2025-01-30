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
package net.codecadenza.eclipse.generator.facade;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map.Entry;
import net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.facade.method.FacadeMethodGeneratorFactory;
import net.codecadenza.eclipse.generator.repository.RepositoryGenerator;
import net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.RepositoryMethodGeneratorFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for facade beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FacadeGenerator extends AbstractJavaSourceGenerator {
	private final BoundaryBean boundaryBean;
	private final Project project;
	private final RepositoryGenerator repositoryGenerator;
	private final String beanName;
	private final HashSet<ServiceBean> addServiceSet = new HashSet<>();
	private boolean addLoggingInterceptor;

	/**
	 * Constructor
	 * @param boundaryBean
	 */
	public FacadeGenerator(BoundaryBean boundaryBean) {
		super(boundaryBean.getBeanSourceFile());

		this.boundaryBean = boundaryBean;
		this.project = boundaryBean.getNamespace().getProject();
		this.repositoryGenerator = new RepositoryGenerator(boundaryBean);

		// We use the meta-model's interface name as class name to avoid changes in subsequent generators!
		this.beanName = boundaryBean.getInterfaceName();

		// Search for additional facades
		for (final Repository repository : repositoryGenerator.getAdditionalRepositories())
			for (final BoundaryBean boundary : project.getAllBoundariesOfProject())
				if (boundary.getRepository().equals(repository))
					this.addServiceSet.add(boundary);

		for (final BoundaryMethod method : boundaryBean.getBoundaryMethods()) {
			final BasicBoundaryMethodGenerator methodGenerator = FacadeMethodGeneratorFactory.getMethodGenerator(method, this);

			if (methodGenerator == null)
				continue;

			if (project.isJavaSEApplication())
				continue;

			if (method.getMethodType() == BoundaryMethodTypeEnumeration.CREATE) {
				for (final MethodParameter p : method.getMethodParameters()) {
					if (!(p.getType() instanceof final DTOBean paramDTO))
						continue;

					if (boundaryBean.getDomainObject().equals(paramDTO.getDomainObject()))
						continue;

					final Collection<BoundaryBean> allBoundaries = project.getAllBoundariesOfProject();

					for (final BoundaryBean boundary : allBoundaries)
						if (boundary.getDomainObject().equals(paramDTO.getDomainObject())) {
							this.addServiceSet.add(boundary);
							break;
						}
				}
			}
			else if (method.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT) {
				final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
				final DataExchangeServiceBean exchangeService = exchangeMethod.getDataExchangeServiceBean();

				this.addServiceSet.add(exchangeService);
			}
		}

		if (project.isJakartaEEApplication() && project.getDomainObjectByTag(DomainTagEnumeration.LOGGING) != null)
			this.addLoggingInterceptor = true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		final String subNamespace = boundaryBean.getDomainObject().getNamespace().getName();
		final String domainNamespace = project.getDomainNamespace().toString();

		importPackage("net.codecadenza.runtime.jpa");

		if (project.isJakartaEEApplication()) {
			importPackage("jakarta.ejb");
			importPackage("jakarta.annotation.security");

			if (project.getDomainObjectByTag(DomainTagEnumeration.LOGGING) != null) {
				importPackage("jakarta.interceptor");
				importClass("net.codecadenza.runtime.server.logging.LoggingInterceptor");
			}
		}
		else if (project.isSpringBootApplication()) {
			importPackage("jakarta.annotation.security");
			importClass("org.springframework.stereotype.Service");

			for (final BoundaryMethod method : boundaryBean.getBoundaryMethods()) {
				final BasicBoundaryMethodGenerator methodGenerator = FacadeMethodGeneratorFactory.getMethodGenerator(method, this);

				if (methodGenerator != null && methodGenerator.isAddTransactionAnnotation()) {
					importPackage("org.springframework.transaction.annotation");
					break;
				}
			}
		}
		else if (project.isJavaSEApplication())
			importPackage("jakarta.persistence");

		if (!boundaryBean.getDomainObject().isAbstract()) {
			if (project.getValidationType() == ValidationTypeEnumeration.INTERNAL)
				importClass("net.codecadenza.runtime.validation.PropertyConstraintViolationException");
			else
				importClass("jakarta.validation.ConstraintViolationException");
		}

		importPackage(domainNamespace + "." + subNamespace);

		addImports(repositoryGenerator.getImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		final DomainAttribute pkAttr = boundaryBean.getDomainObject().getPKAttribute();

		if (pkAttr.getJavaType().getNamespace() != null)
			importPackage(pkAttr.getJavaType().getNamespace().toString());

		if (project.isJakartaEEApplication()) {
			b.append("@Stateless\n");

			if (addLoggingInterceptor)
				b.append("@Interceptors(LoggingInterceptor.class)\n");
		}
		else if (project.isSpringBootApplication())
			b.append("@Service\n");

		b.append("public class " + beanName);
		b.append(" extends AbstractRepository");
		b.append("<" + boundaryBean.getDomainObject().getName() + ", ");
		b.append(pkAttr.getJavaType().getWrapperTypeName());
		b.append(">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();

		// Add constants for the query parameters
		for (final Entry<String, String> entrySet : repositoryGenerator.getQueryParameterConstants().entrySet())
			addPrivateConstant(JavaType.STRING, entrySet.getKey(), "\"" + entrySet.getValue() + "\"").create();

		// Check if this boundary bean holds the log-on method and add the role names as constants
		if (logOnBoundary != null && logOnBoundary.equals(boundaryBean))
			project.getRoles().forEach(role -> addPublicConstant(JavaType.STRING, "ROLE_" + role.getName().toUpperCase(),
					"\"" + role.getName().toUpperCase() + "\"").create());

		addServiceSet.forEach(service -> {
			final String typeName;
			final String fieldName;

			if (!service.getNamespace().toString().equals(boundaryBean.getNamespace().toString()))
				importPackage(service.getNamespace().toString());

			if (service instanceof DataExchangeServiceBean) {
				typeName = service.getName();
				fieldName = service.getName().substring(0, 1).toLowerCase() + service.getName().substring(1);
			}
			else {
				typeName = service.getInterfaceName();
				fieldName = service.getInterfaceName().substring(0, 1).toLowerCase() + service.getInterfaceName().substring(1);
			}

			if (project.isJakartaEEApplication() || project.isSpringBootApplication())
				addPrivateField(typeName, fieldName).inject().create();
			else
				addPrivateField(typeName, fieldName).create();
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		if (project.isJavaSEApplication()) {
			var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" * @param em\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + beanName + "(EntityManager em)\n");
			b.append("{\n");
			b.append("super(em);\n");

			if (!addServiceSet.isEmpty())
				b.append("\n");

			for (final ServiceBean service : addServiceSet) {
				final String serviceBeanName = service.getInterfaceName().substring(0, 1).toLowerCase()
						+ service.getInterfaceName().substring(1);
				final String serviceClassName = service.getInterfaceName();

				b.append("this." + serviceBeanName + " = new " + serviceClassName + "(em);\n");
			}

			b.append("}\n\n");

			addConstructor(beanName + "(EntityManager em)", b.toString());

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Default constructor\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + beanName + "()\n");
			b.append("{\n");
			b.append("this(PersistenceEngine.getEntityManager());\n");
			b.append("}\n\n");

			addConstructor(beanName + "()", b.toString());
		}
		else
			super.addConstructors();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		// Add all methods of the respective repository
		for (final RepositoryMethod method : boundaryBean.getRepository().getRepositoryMethods()) {
			if (method.isGenerationOmitted() || method.getMethodType() == RepositoryMethodTypeEnumeration.GET_ASSOCIATION)
				continue;

			final BasicRepositoryMethodGenerator generator = RepositoryMethodGeneratorFactory.getMethodGenerator(method, this);

			addMethod(generator.getMethodSignature(), generator.createMethod());

			addImports(generator.getImports());
		}

		// Add imports
		for (final BoundaryMethod method : boundaryBean.getBoundaryMethods()) {
			final BasicBoundaryMethodGenerator methodGenerator = FacadeMethodGeneratorFactory.getMethodGenerator(method, this);

			if (methodGenerator == null)
				continue;

			addImports(methodGenerator.getImports());
		}

		// Add methods
		for (final BoundaryMethod method : boundaryBean.getBoundaryMethods()) {
			final BasicBoundaryMethodGenerator methodGenerator = FacadeMethodGeneratorFactory.getMethodGenerator(method, this);

			if (methodGenerator == null)
				continue;

			final String identifier = methodGenerator.getMethodSignature(false);

			final var b = new StringBuilder();
			b.append(methodGenerator.createComment());
			b.append(methodGenerator.createMethod());

			addMethod(identifier, b.toString());
		}

		if (project.isJavaSEApplication()) {
			final var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Close entity manager\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void closeEntityManager()\n");
			b.append("{\n");
			b.append("if(em != null)\n");
			b.append("PersistenceEngine.releaseEntityManager(em);\n");
			b.append("}\n\n");

			addMethod("void closeEntityManager()", b.toString());
		}
	}

}
