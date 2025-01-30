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
package net.codecadenza.runtime.jpa;

import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaDelete;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import net.codecadenza.runtime.jpa.util.JPAConstructorQueryStatementGenerator;
import net.codecadenza.runtime.jpa.util.JPAQueryStatementGenerator;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.exception.GeneralSearchException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for all repositories. This class can be used in managed and unmanaged environments!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of an entity a specific repository should work with
 * @param <P> the type of the primary key attribute
 */
public abstract class AbstractRepository<T, P> {
	// The default number of returned items for small lists
	public static final int SMALL_LIST_SIZE = 50;
	// The default number of returned items
	public static final int DEFAULT_LIST_SIZE = 1000;
	// The maximum number of returned items if no upper limit has been defined
	public static final int MAX_LIST_SIZE = 10000;
	public static final String WILDCARD = "%";

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	protected @PersistenceContext EntityManager em;
	protected Class<T> entityType;

	/**
	 * Default constructor
	 */
	@SuppressWarnings("unchecked")
	protected AbstractRepository() {
		Type type = getClass().getGenericSuperclass();

		if (!(type instanceof ParameterizedType))
			type = getClass().getSuperclass().getGenericSuperclass();

		this.entityType = (Class<T>) ((ParameterizedType) type).getActualTypeArguments()[0];
	}

	/**
	 * Constructor
	 * @param em
	 */
	protected AbstractRepository(EntityManager em) {
		this();

		this.em = em;
	}

	/**
	 * @return the entity manager
	 */
	public EntityManager getEntityManager() {
		return em;
	}

	/**
	 * Find and attach an entity that has the provided ID
	 * @param id the ID of the entity to search for
	 * @return the entity or null if it could not be found
	 */
	public T findById(P id) {
		return em.find(entityType, id);
	}

	/**
	 * Find and attach an entity that has the provided ID
	 * @param id the ID of the entity to search for
	 * @param mustExist
	 * @throws EntityNotFoundException if the entity could not be found and the parameter <code>mustExist</code> is true
	 * @return the entity with the provided ID or null if it could not be found
	 */
	public T findById(P id, boolean mustExist) {
		final T entity = findById(id);

		if (entity != null)
			return entity;

		if (mustExist)
			throw new EntityNotFoundException("An entity of type '" + entityType.getName() + "' with ID '" + id + "' doesn't exist!");

		return null;
	}

	/**
	 * Find and attach an entity by using the given type and the primary key
	 * @param <X> the type of the entity to be returned
	 * @param type defines the expected return type
	 * @param id the ID of the entity to search for
	 * @return the attached entity or null if it could not be found
	 */
	public <X> X findById(Class<X> type, Object id) {
		return em.find(type, id);
	}

	/**
	 * Get all entities
	 * @return a list that contains all entities
	 */
	public List<T> findAll() {
		final CriteriaBuilder criteriaBuilder = em.getCriteriaBuilder();
		final CriteriaQuery<T> criteriaQuery = criteriaBuilder.createQuery(entityType);
		final Root<T> root = criteriaQuery.from(entityType);
		final CriteriaQuery<T> query = criteriaQuery.select(root);

		return em.createQuery(query).getResultList();
	}

	/**
	 * Check if an entity with the provided ID exists
	 * @param id the ID of the entity to search for
	 * @return true if the entity with this ID could be found
	 */
	public boolean existsById(P id) {
		return findById(id) != null;
	}

	/**
	 * Get an entity instance whose state may be lazily fetched
	 * @param id the ID to search for
	 * @throws EntityNotFoundException if the entity doesn't exist
	 * @return the entity reference
	 */
	public T getReference(P id) {
		return em.getReference(entityType, id);
	}

	/**
	 * Get an entity instance whose state may be lazily fetched
	 * @param <X> the type of the entity to be returned
	 * @param type defines the expected return type
	 * @param id the ID of the entity to search for
	 * @throws EntityNotFoundException if the entity doesn't exist
	 * @return the entity reference
	 */
	public <X> X getReference(Class<X> type, Object id) {
		return em.getReference(type, id);
	}

	/**
	 * Persist the given entity
	 * @param entity the entity to be persisted
	 */
	public void persist(T entity) {
		em.persist(entity);
	}

	/**
	 * Persist the given entity
	 * @param entity the entity to be persisted
	 * @param performFlush flag that controls if the database synchronization should be performed immediately
	 * @param performRefresh flag that controls if a refresh operation should be performed after persist
	 * @return the persisted entity
	 */
	public T persist(T entity, boolean performFlush, boolean performRefresh) {
		em.persist(entity);

		if (performFlush)
			em.flush();

		if (performRefresh)
			em.refresh(entity);

		return entity;
	}

	/**
	 * Persist a list of entities
	 * @param entities a list of entities to be persisted
	 */
	public void persist(List<T> entities) {
		entities.stream().forEach(this::persist);
	}

	/**
	 * Merge the given entity
	 * @param entity the entity to be merged
	 * @return the merged entity instance
	 */
	public T merge(T entity) {
		return em.merge(entity);
	}

	/**
	 * Merge the given entity
	 * @param entity the entity to be merged
	 * @param performFlush flag that controls if the database synchronization should be performed immediately
	 * @return the merged entity instance
	 */
	public T merge(T entity, boolean performFlush) {
		em.merge(entity);

		if (performFlush)
			em.flush();

		return entity;
	}

	/**
	 * Merge a list of entities
	 * @param entities a list of entities to be merged
	 * @return the list of merged entities
	 */
	public List<T> merge(List<T> entities) {
		return entities.stream().map(this::merge).toList();
	}

	/**
	 * Delete the given entity
	 * @param entity the entity to be deleted
	 */
	public void deleteEntity(T entity) {
		em.remove(entity);
	}

	/**
	 * Delete the given entity
	 * @param id the ID of the entity to be deleted
	 * @throws EntityNotFoundException if the entity doesn't exist
	 */
	public void delete(P id) {
		final T entityToDelete = findById(id, true);

		em.remove(entityToDelete);
	}

	/**
	 * Delete a list of entities
	 * @param entities a list of entities that should be deleted
	 * @throws EntityNotFoundException if at least one entity in the list doesn't exist
	 */
	public void delete(Collection<T> entities) {
		entities.forEach(this::deleteEntity);
	}

	/**
	 * Delete all persistent entities
	 */
	public void deleteAll() {
		final CriteriaBuilder criteriaBuilder = em.getCriteriaBuilder();

		final CriteriaDelete<T> criteriaDelete = criteriaBuilder.createCriteriaDelete(entityType);
		criteriaDelete.from(entityType);

		em.createQuery(criteriaDelete).executeUpdate();
	}

	/**
	 * Search for entities by using the given query statement
	 * @param statement the query statement to be executed
	 * @return a list of entities that match the filter criteria
	 */
	public List<T> search(String statement) {
		return search(statement, MAX_LIST_SIZE);
	}

	/**
	 * Search for entities by using the given query statement and limit the number of results
	 * @param statement the query statement to be executed
	 * @param maxResults the maximum number of entities to be returned
	 * @return a list of entities that match the filter criteria
	 */
	public List<T> search(String statement, int maxResults) {
		return search(statement, maxResults, 0, null);
	}

	/**
	 * Search for entities by using the given query statement with paging and parameters
	 * @param statement the query statement to be executed
	 * @param maxResults the maximum number of entities to be returned
	 * @param startIndex the position of the first result to retrieve
	 * @param addParams a map that contains the values for all named query parameters
	 * @return a list of entities that match the filter criteria
	 */
	public List<T> search(String statement, int maxResults, int startIndex, Map<String, Object> addParams) {
		final TypedQuery<T> query = em.createQuery(statement, entityType);
		query.setMaxResults(maxResults);
		query.setFirstResult(startIndex);

		if (addParams != null)
			addParams.keySet().forEach(param -> query.setParameter(param, addParams.get(param)));

		return query.getResultList();
	}

	/**
	 * Search for entities by using the given query statement
	 * @param <X> the type parameter
	 * @param statement the query statement to be executed
	 * @param type defines the expected return type
	 * @return a list of entities that match the filter criteria
	 */
	public <X> List<X> search(String statement, Class<X> type) {
		return search(statement, MAX_LIST_SIZE, 0, null, type);
	}

	/**
	 * Search for entities by using the given query statement
	 * @param <X> the type parameter
	 * @param statement the query statement to be executed
	 * @param maxResults the maximum number of entities to be returned
	 * @param type defines the expected return type
	 * @return a list of entities that match the filter criteria
	 */
	public <X> List<X> search(String statement, int maxResults, Class<X> type) {
		return search(statement, maxResults, 0, null, type);
	}

	/**
	 * Search for entities
	 * @param <X> the type parameter
	 * @param statement the query statement to be executed
	 * @param maxResults the maximum number of entities to be returned
	 * @param startIndex the position of the first result to retrieve
	 * @param addParams a map that contains the values for all named query parameters
	 * @param type defines the expected return type
	 * @return a list of entities that match the filter criteria
	 */
	public <X> List<X> search(String statement, int maxResults, int startIndex, Map<String, Object> addParams, Class<X> type) {
		final TypedQuery<X> query = em.createQuery(statement, type);
		query.setMaxResults(maxResults);
		query.setFirstResult(startIndex);

		if (addParams != null)
			addParams.forEach(query::setParameter);

		return query.getResultList();
	}

	/**
	 * Search for entities by using the given {@link SearchDTO}
	 * @param <X> the type parameter
	 * @param searchObj an object that contains all information for building the query statement
	 * @param type defines the expected return type
	 * @return a list of entities that match the filter criteria
	 * @throws GeneralSearchException if the search operation has failed
	 */
	public <X> List<X> search(SearchDTO searchObj, Class<X> type) {
		return search(searchObj, type, null);
	}

	/**
	 * Search for entities by using the given {@link SearchDTO}
	 * @param <X> the type parameter
	 * @param searchObj an object that contains all information for building the query statement
	 * @param type defines the expected return type
	 * @param selectTokens an optional list with select tokens that is used to create a JPA constructor query
	 * @return a list of entities that match the filter criteria
	 * @throws GeneralSearchException if the search operation has failed
	 */
	public <X> List<X> search(SearchDTO searchObj, Class<X> type, List<String> selectTokens) {
		if (selectTokens != null) {
			final var queryGenerator = new JPAConstructorQueryStatementGenerator();
			queryGenerator.setClassName(type.getName());
			queryGenerator.setSelectTokens(selectTokens);

			searchObj.setFromClause(queryGenerator.createStatement() + searchObj.getFromClause().trim());
		}

		final String statement = JPAQueryStatementGenerator.createStatement(searchObj);

		try {
			final Map<String, Object> addParams = JPAQueryStatementGenerator.createParameters(searchObj);

			return search(statement, searchObj.getMaxResult(), searchObj.getStartIndex(), addParams, type);
		}
		catch (final Exception e) {
			logger.error("Error while performing query with statement '{}'!", statement, e);

			throw new GeneralSearchException(e, true);
		}
	}

	/**
	 * Count the number of all persistent entities
	 * @return the count result
	 */
	public long count() {
		final CriteriaBuilder criteriaBuilder = em.getCriteriaBuilder();

		final CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		criteriaQuery.select(criteriaBuilder.count(criteriaQuery.from(entityType)));

		return em.createQuery(criteriaQuery).getSingleResult();
	}

	/**
	 * Perform a count operation by using the given {@link SearchDTO}
	 * @param searchObj an object that contains all information for building the query statement
	 * @return the count result
	 * @throws IllegalStateException if the values of the named query parameters could not be parsed
	 */
	public long count(SearchDTO searchObj) {
		final String statement = JPAQueryStatementGenerator.createCountStatement(searchObj);
		final var errorMsg = "Error while parsing additional query parameters!";
		Map<String, Object> addParams = null;

		try {
			addParams = JPAQueryStatementGenerator.createParameters(searchObj);
		}
		catch (final Exception e) {
			logger.error(errorMsg, e);
		}

		if (addParams == null)
			throw new IllegalStateException(errorMsg);

		final TypedQuery<Long> query = em.createQuery(statement, Long.class);

		addParams.forEach(query::setParameter);

		return query.getSingleResult();
	}

}
